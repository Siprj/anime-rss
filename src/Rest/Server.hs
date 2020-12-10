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
    ( RssApi
    , Context(..)
    , rssApiHandler
    , protectedHandlers
    , apiHander
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Control.Monad.Freer (send, Eff)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just), maybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , rfc822DateFormat
    )
import Servant ((:<|>)((:<|>)), URI, throwError, err404)
import Servant.API ((:>), Get, NoContent)
import Servant.Server (Handler, ServerT)
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
import System.IO (IO)

import Core.Type.Episode (Episode(Episode, title, url, date, imageUrl, number))
import qualified Core.Type.User as Core (User(User, userId, email, name, episodeChannel))
import DataModel.Service (DataModel, listAllFeeds)
import Rest.AtomMime (AtomFeed)
import Rest.Api (ChannelId, Api, Protected, User(User, userId, email, name, episodeChannel), Anime, PostAnimeFollow, Login)
import Rest.Authentication ()
import Prelude (undefined)
import Servant.Auth.Server (AuthResult)
import Servant.Auth.Server
    (AuthResult(NoSuchUser, Authenticated, BadPassword, Indefinite))
import Servant (ServerError)
import Servant (Headers(Headers))
import Servant (Header)
import Web.Cookie (SetCookie)


data Context = Context
    { baseUri :: URI
    , title :: Text
    }

type Handler' = Eff [Reader Context, DataModel, IO, Handler]
type RestServer api = ServerT api Handler'

type RssApi = "atom" :> Get '[AtomFeed] Feed

rssApiHandler :: RestServer RssApi
rssApiHandler = do
    (feeds, lastModification) <- listAllFeeds
    context@Context{..} <- ask

    pure . feedFromAtom . fd context lastModification
        $ fmap toEntry feeds

  where
    fd :: Context -> Maybe UTCTime -> [Entry] -> Atom.Feed
    fd Context{..} date entries = fd'
        { feedLinks = [nullLink $ showT baseUri]
        , feedEntries = entries
        }
      where
          fd' = nullFeed (showT baseUri) (TextString title) . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Episode -> Entry
    toEntry Episode{..} = toEntry'
        { entryLinks = [nullLink $ showT url]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
            $ "<div><a href=\"" <> showT url <> "\"><img src=\""
            <> showT imageUrl <> "\"></div>"
        }
      where
        toEntry' :: Entry
        toEntry' = nullEntry (showT url)
            (TextString $ title <> " [Episode: " <> showT number <> "]")
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

showT :: Show a => a -> Text
showT = pack . show

throw :: ServerError -> Handler a
throw = throwError

throwError' :: ServerError -> Handler' a
throwError' = send . throw

protectedHandlers :: Core.User -> RestServer Protected
protectedHandlers user = go
  where
    go = userGetHandler user
        :<|> animeUpdateHandler user
        :<|> animeGetHandler user

userGetHandler :: Core.User -> Handler' User
userGetHandler Core.User {..} = pure $ User {..}

animeUpdateHandler :: Core.User -> [PostAnimeFollow] -> Handler' [Anime]
animeUpdateHandler Core.User{..} postAnimeFollow = do
    pure

animeGetHandler :: Core.User -> Handler' [Anime]
animeGetHandler = undefined

loginHandler :: Login -> Handler' (Headers '[Header "Set-Cookie" SetCookie] NoContent)
loginHandler = undefined

atomGetHandler :: Handler' Feed
atomGetHandler = undefined

atomEpisodesGetHandler :: ChannelId -> Handler' Feed
atomEpisodesGetHandler = undefined

atomAnimeGetHandler :: ChannelId -> Handler' Feed
atomAnimeGetHandler = undefined

apiHander :: RestServer Api
apiHander = atomGetHandler
    :<|> atomEpisodesGetHandler
    :<|> atomAnimeGetHandler
    :<|> loginHandler
    :<|> protectedHandlers
