{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module AnimeRss.Rest.Server
    ( Context(..)
    , apiHander
    --, protectedHandlers
    )
  where

import Relude (show)
import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , rfc822DateFormat
    )
import Servant ((:<|>)((:<|>)), Header, Headers, URI, NoContent(NoContent), ServerT, ServerError, err401, err409)
import Servant.Auth.Server ( SetCookie, AuthResult (..), CookieSettings, JWTSettings, acceptLogin )
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

import qualified AnimeRss.DataModel.Types as Core
import AnimeRss.Rest.Api (ChannelId, Api, Protected, Login (..), LoggedInUser (LoggedInUser, userId), User(..), PostAnimeFollow(..), Anime(..), SubParam(..))
import AnimeRss.Rest.Authentication ()
import Effectful ( Eff, (:>), IOE, MonadIO (..) )
import Effectful.Error.Dynamic ( Error, throwError )
import Effectful.Reader.Dynamic ( ask, Reader )
import DBE ( PostgreSql )
import Control.Monad ((>>=))
import AnimeRss.DataModel.Queries (listAnimes, selectUserByEmail, listEpisodesByChannelId, getDbUserById, insertUserFollows, deleteUserFollows, listUserRelatedAnime)
import Crypto.Error (CryptoFailable(..))
import Crypto.PasswordStore
import Data.Text.Encoding
import System.IO
import AnimeRss.DataModel.Types (toPasswordHash, CreateUserFollows(..), DeleteUserFollows(..))


data Context = Context
    { baseUri :: URI
    , cookiesSettings :: CookieSettings
    , jwtSettings :: JWTSettings
    }

type Handler' es = (Error ServerError :> es, Reader Context :> es, PostgreSql :> es)
type RestServer api es = ServerT api (Eff es)

maybeThrow :: Handler' es => ServerError -> Maybe a -> Eff es a
maybeThrow e = \case
    Nothing -> throwError e
    Just v -> pure v

authHelper :: (Handler' es) => AuthResult LoggedInUser -> Eff es LoggedInUser
authHelper = \case
  BadPassword -> throwError err401
  NoSuchUser -> throwError err401
  Indefinite -> throwError err401
  Authenticated user -> pure user

protectedHandlers :: (Handler' es, IOE :> es) => AuthResult LoggedInUser -> RestServer Protected es
protectedHandlers user = go
  where
    go = userGetHandler user
        :<|> animeUpdateHandler user
        :<|> animeListHandler user

userGetHandler :: (Handler' es) => AuthResult LoggedInUser  -> Eff es User
userGetHandler authUsr = do
    loggedInUser <- authHelper authUsr
    getDbUserById loggedInUser.userId >>= maybeThrow err409 . fmap toUser
  where
    toUser Core.User{..} = User
        { userId = id
        , email
        , name
        , episodeChannel = newsChannel
        }

animeUpdateHandler :: (Handler' es, IOE :> es) => AuthResult LoggedInUser  -> PostAnimeFollow -> Eff es NoContent
animeUpdateHandler authUsr follow = do
    loggedInUser <- authHelper authUsr
    if follow.follow
        then insertUserFollows $ CreateUserFollows loggedInUser.userId follow.animeId
        else deleteUserFollows $ DeleteUserFollows loggedInUser.userId follow.animeId
    pure NoContent

animeListHandler :: (Handler' es, IOE :> es) => AuthResult LoggedInUser -> Maybe SubParam -> Maybe Text -> Eff es [Anime]
animeListHandler authUrs mSubParam mSearch = do
    loggedInUser <- authHelper authUrs
    animeList <- listUserRelatedAnime loggedInUser.userId (fromMaybe All mSubParam) mSearch
    pure $ fmap toApiAnime animeList
  where
    toApiAnime Core.UserRelatedAnime{..} = Anime
        { animeId
        , title
        , url = url
        , imageUrl = imageUrl
        , date
        , following
        }

loginHandler :: (Handler' es, IOE :> es) => Login -> Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler loginData@Login{..} = do
    liftIO . putStrLn $ "login handler: " <> show loginData
    user <- selectUserByEmail email >>= maybe (throwError err401) pure
    liftIO . putStrLn $ "selected user: " <> show user
    case verifyPassword (encodeUtf8 password) . toPasswordHash $ user.password of
        CryptoPassed v -> if v
            then pure ()
            else throwError err401
        -- TODO: Better logging???
        CryptoFailed e -> do
            liftIO $ print e
            throwError err401
    Context{..} <- ask

    mApplyCookies <- liftIO . acceptLogin cookiesSettings jwtSettings $ toLoggedInUser user
    case mApplyCookies of
      Nothing           -> throwError err401
      Just applyCookies -> pure $ applyCookies NoContent
  where
    toLoggedInUser :: Core.User -> LoggedInUser
    toLoggedInUser Core.User{id} = LoggedInUser id

atomEpisodesGetHandler :: (Handler' es, IOE :> es) => ChannelId -> Eff es Feed
atomEpisodesGetHandler channelId = do
    context <- ask
    episodes <- listEpisodesByChannelId channelId

    let lastModification = Nothing

    pure . feedFromAtom . fd context lastModification
        $ fmap toEntry episodes
  where
    fd :: Context -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd Context{..} date entries = fd'
        { feedLinks = [nullLink $ show baseUri]
        , feedEntries = entries
        }
      where
          fd' = nullFeed (show baseUri) (TextString "episode channel") . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Episode -> Entry
    toEntry Core.Episode{..} = entry'
        { entryLinks = [nullLink $ show url]
        , entrySummary = Just . HTMLString
            $ "<div><a href=\"" <> show url <> "\"><img src=\""
            <> show imageUrl <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' = nullEntry (show url)
            (TextString $ title <> " [Episode: " <> number <> "]")
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date



atomAnimeGetHandler :: (Handler' es, IOE :> es) => Eff es Feed
atomAnimeGetHandler = do
    animes <- listAnimes
    context <- ask

    let lastModification = Nothing

    pure . feedFromAtom . fd context lastModification
        $ fmap toEntry animes

  where
    fd :: Context -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd Context{..} date entries = fd'
        { feedLinks = [nullLink $ show baseUri]
        , feedEntries = entries
        }
      where
          fd' = nullFeed (show baseUri) (TextString "anime channel") . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Anime -> Entry
    toEntry Core.Anime{..} = entry'
        { entryLinks = [nullLink $ show url]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
        -- TODO: Add link to the server with possibility to follow the anime
            $ "<div><a href=\"" <> show url <> "\"><img src=\""
            <> show imgUrl <> "\"></div>"
        }
      where
        entry' :: Entry
        entry' = nullEntry (show url)
            (TextString title)
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

apiHander :: (Handler' es, IOE :> es) => RestServer Api es
apiHander = atomEpisodesGetHandler
    :<|> atomAnimeGetHandler
    :<|> loginHandler
    :<|> protectedHandlers
