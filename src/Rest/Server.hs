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

import Control.Applicative (pure)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , rfc822DateFormat
    )
import Servant ((:<|>)((:<|>)), URI, NoContent, Handler, ServerT)
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

import qualified Core.Type.Anime as Core (Anime(Anime, title, url, date, imageUrl))
import qualified Core.Type.User as Core (User(User, userId, email, name, episodeChannel))
import DataModel.Service (DataModel, listAnime)
import Rest.Api (ChannelId, Api, Protected, User(User, userId, email, name, episodeChannel), Anime, PostAnimeFollow, Login)
import Rest.Authentication ()
import Prelude (undefined)
import Servant (Headers)
import Servant (Header)
import Web.Cookie (SetCookie)


newtype Context = Context
    { baseUri :: URI
    }

type Handler' = Eff [Reader Context, DataModel, IO, Handler]
type RestServer api = ServerT api Handler'

showT :: Show a => a -> Text
showT = pack . show

-- throw :: ServerError -> Handler a
-- throw = throwError
--
-- throwError' :: ServerError -> Handler' a
-- throwError' = send . throw

protectedHandlers :: Core.User -> RestServer Protected
protectedHandlers user = go
  where
    go = userGetHandler user
        :<|> animeUpdateHandler user
        :<|> animeGetHandler user

userGetHandler :: Core.User -> Handler' User
userGetHandler Core.User {..} = pure $ User {..}

animeUpdateHandler :: Core.User -> [PostAnimeFollow] -> Handler' [Anime]
animeUpdateHandler = undefined

animeGetHandler :: Core.User -> Handler' [Anime]
animeGetHandler = undefined

loginHandler :: Login -> Handler' (Headers '[Header "Set-Cookie" SetCookie] NoContent)
loginHandler = undefined

atomEpisodesGetHandler :: ChannelId -> Handler' Feed
atomEpisodesGetHandler = undefined

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
          fd' = nullFeed (showT baseUri) (TextString "anime feed") . pack
            $ maybe "" (formatTime defaultTimeLocale rfc822DateFormat) date

    toEntry :: Core.Anime -> Entry
    toEntry Core.Anime{..} = entry'
        { entryLinks = [nullLink $ showT url]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
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
--    :<|> protectedHandlers
