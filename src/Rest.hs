{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Rest
    ( RssApi
    , Context(..)
    , rssApiHandler
    )
  where

import Control.Applicative (pure)
import Control.Monad.Freer (Eff)
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
import Network.URI (URI)
import Servant.API ((:>), Get)
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

import qualified DataModel.Persistent as DataModel
    (Feed(Feed, feedName, feedUrl, feedDate, feedImgUrl, feedEpisodeNumber))
import DataModel.Service (DataModel, listFeeds)
import Rest.AtomMime (AtomFeed)


data Context = Context
    { baseUri :: URI
    , title :: Text
    }

type Handler' = Eff [Reader Context, DataModel, IO, Handler]
type RestServer api = ServerT api Handler'

type RssApi = "atom" :> Get '[AtomFeed] Feed

rssApiHandler :: RestServer RssApi
rssApiHandler = do
    (feeds, lastModification) <- listFeeds
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

    toEntry :: DataModel.Feed -> Entry
    toEntry DataModel.Feed{..} = toEntry'
        { entryLinks = [nullLink $ showT feedUrl]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
            $ "<div><a href=\"" <> showT feedUrl <> "\"><img src=\""
            <> showT feedImgUrl <> "\"></div>"
        }
      where
        toEntry' :: Entry
        toEntry' = nullEntry (showT feedUrl)
            (TextString $ feedName <> " [Episode: " <> showT feedEpisodeNumber <> "]")
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat feedDate

showT :: Show a => a -> Text
showT = pack . show
