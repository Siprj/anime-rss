{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rest.Server
    ( Context(..)
    , apiHander
    , protectedHandlers
    )
  where

import Crypto.PasswordStore (verifyPassword)
import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Freer (Eff, send)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , rfc822DateFormat
    )
import Servant ((:<|>)((:<|>)), Header, Headers, URI, NoContent, Handler, ServerT, ServerError, throwError, err401, err409)
import Servant.Auth.Server ( SetCookie, AuthResult (Indefinite, NoSuchUser, BadPassword, Authenticated), acceptLogin, CookieSettings, JWTSettings )
import Text.Atom.Feed
    ( Entry(entryLinks, entrySummary)
    , TextContent(TextString, HTMLString)
    , feedEntries
    , feedLinks
    , nullEntry
    , nullFeed
    , nullLink
    )
import qualified Text.Atom.Feed as Atom (Feed)
import Text.Feed.Constructor (feedFromAtom)
import Text.Feed.Types (Feed)
import Text.Show (Show, show)
import System.IO (IO, print)

import qualified Core.Type.Episode as Core (Episode(Episode, title, url, imageUrl, date, number))
import qualified Core.Type.Anime as Core (Anime(Anime, title, url, date, imageUrl), UserRelatedAnime(UserRelatedAnime, animeId, title, url, imageUrl, date, following))
import qualified Core.Type.User as Core (User(User, userId, email, name, episodeChannel))
import qualified Core.Type.UserFollow as Core (UserFollow(UserFollow, userId, animeId, follow))
import DataModel.Service (DataModel, listAnime, selectUserByEmail, listUserRelatedAnime, selectUser, modifyUsersAnime, listChannelEpisodes)
import Rest.Api (ChannelId, Api, Protected, Anime(Anime, animeId, title, url, imageUrl, date, following), PostAnimeFollow(PostAnimeFollow, animeId, follow), Login(Login, email, password), LoggedInUser(LoggedInUser, userId), User(User, userId, email, name, episodeChannel))
import Rest.Authentication ()
import Servant.API (NoContent(NoContent))
import Crypto.Error (CryptoFailable(CryptoPassed, CryptoFailed))
import Optics ((^.))


data Context = Context
    { baseUri :: URI
    , cookiesSettings :: CookieSettings
    , jwtSettings :: JWTSettings
    }

type Handler' = Eff [Reader Context, DataModel, IO, Handler]
type RestServer api = ServerT api Handler'

maybeThrow :: ServerError -> Maybe a -> Handler' a
maybeThrow e = \case
    Nothing -> throwError' e
    Just v -> pure v

showT :: Show a => a -> Text
showT = pack . show

throw :: ServerError -> Handler a
throw = throwError

throwError' :: ServerError -> Handler' a
throwError' = send . throw

authHelper :: AuthResult LoggedInUser -> Handler' LoggedInUser
authHelper = \case
  BadPassword -> throwError' err401
  NoSuchUser -> throwError' err401
  Indefinite -> throwError' err401
  Authenticated user -> pure user

protectedHandlers :: AuthResult LoggedInUser -> RestServer Protected
protectedHandlers user = go
  where
    go = userGetHandler user
        :<|> animeUpdateHandler user
        :<|> animeListHandler user

userGetHandler :: AuthResult LoggedInUser  -> Handler' User
userGetHandler authUsr = do
    LoggedInUser{..} <- authHelper authUsr
    selectUser userId >>= maybeThrow err409 . fmap toUser
  where
    toUser Core.User{..} = User
        { userId
        , email
        , name
        , episodeChannel
        }

animeUpdateHandler :: AuthResult LoggedInUser  -> [PostAnimeFollow] -> Handler' NoContent
animeUpdateHandler authUsr follows = do
    LoggedInUser{..} <- authHelper authUsr
    modifyUsersAnime $ fmap (toAnimeFollow userId) follows
    pure NoContent
  where
    toAnimeFollow userId PostAnimeFollow{..} = Core.UserFollow
        { userId
        , animeId
        , follow
        }

animeListHandler :: AuthResult LoggedInUser  -> Handler' [Anime]
animeListHandler authUrs = do
    LoggedInUser{..} <- authHelper authUrs
    animeList <- listUserRelatedAnime userId
    pure $ fmap toApiAnime animeList
  where
    toApiAnime Core.UserRelatedAnime{..} = Anime
        { animeId
        , title
        , url = showT url
        , imageUrl = showT imageUrl
        , date
        , following
        }

loginHandler :: Login -> Handler' (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler Login{..} = do
    user <- selectUserByEmail email >>= maybe (throwError' err401) pure
    case verifyPassword (encodeUtf8 password) $ user ^. #password  of
        CryptoPassed v -> if v
            then pure ()
            else throwError' err401
        -- TODO: Better logging???
        CryptoFailed e -> do
            send $ print e
            throwError' err401
    Context{..} <- ask

    mApplyCookies <- send . acceptLogin cookiesSettings jwtSettings $ toLoggedInUser user
    case mApplyCookies of
      Nothing           -> throwError' err401
      Just applyCookies -> pure $ applyCookies NoContent
  where
    toLoggedInUser :: Core.User -> LoggedInUser
    toLoggedInUser Core.User{userId} = LoggedInUser {..}

atomEpisodesGetHandler :: ChannelId -> Handler' Feed
atomEpisodesGetHandler channelId = do
    context <- ask
    episodes <- listChannelEpisodes channelId

    let lastModification = Nothing

    pure . feedFromAtom . fd context lastModification
        $ fmap toEntry episodes
  where
    fd :: Context -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd Context{..} date entries = fd'
        { feedLinks = [nullLink $ showT baseUri]
        , feedEntries = entries
        }
      where
          fd' = nullFeed (showT baseUri) (TextString "episode channel") . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Episode -> Entry
    toEntry Core.Episode{..} = entry'
        { entryLinks = [nullLink $ showT url]
        , entrySummary = Just . HTMLString
            $ "<div><a href=\"" <> showT url <> "\"><img src=\""
            <> showT imageUrl <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' = nullEntry (showT url)
            (TextString $ title <> " [Episode: " <> number <> "]")
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

atomAnimeGetHandler :: Handler' Feed
atomAnimeGetHandler = do
    animes <- listAnime
    context <- ask

    let lastModification = Nothing

    pure . feedFromAtom . fd context lastModification
        $ fmap toEntry animes

  where
    fd :: Context -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd Context{..} date entries = fd'
        { feedLinks = [nullLink $ showT baseUri]
        , feedEntries = entries
        }
      where
          fd' = nullFeed (showT baseUri) (TextString "anime channel") . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Anime -> Entry
    toEntry Core.Anime{..} = entry'
        { entryLinks = [nullLink $ showT url]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
        -- TODO: Add link to the server with possibility to follow the anime
            $ "<div><a href=\"" <> showT url <> "\"><img src=\""
            <> showT imageUrl <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' = nullEntry (showT url)
            (TextString title)
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

apiHander :: RestServer Api
apiHander = atomEpisodesGetHandler
    :<|> atomAnimeGetHandler
    :<|> loginHandler
    :<|> protectedHandlers
