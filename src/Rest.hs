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
import Control.Exception (Exception, throw)
import Control.Monad (Monad)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.ByteString.Lazy (ByteString)
import Data.Default(def)
import Data.Either
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Proxy(Proxy)
import Data.Set (Set)
import Data.String (String)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text, pack)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    , rfc822DateFormat
    )
import Data.Typeable (Typeable)
import Data.Vector (toList)
import Network.HTTP.Media ((//))
import Network.URI (URI)
import Servant.API ((:>), Get)
import Servant.API.ContentTypes
    ( Accept(contentType)
    , MimeRender(mimeRender)
    , MimeUnrender(mimeUnrender)
    )
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
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Text.Show (Show, show)
import Text.XML
    ( Document(Document, documentEpilogue, documentPrologue, documentRoot)
    , Prologue(Prologue, prologueAfter, prologueBefore, prologueDoctype)
    , fromXMLElement
    , renderText
    )
import System.IO (IO)

import qualified DataModel.Type.Feed as DataModel
    (Feed(Feed, name, url, date, imgUrl, episodeNumber))
import DataModel.Service (DataModel, listFeeds)


data Context = Context
    { baseUri :: URI
    , title :: Text
    }

type Handler' = Eff [Reader Context, DataModel, IO, Handler]
type RestServer api = ServerT api Handler'

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight _ (Just a) = Right a
maybeToRight err Nothing = Left err

data AtomFeed
  deriving (Typeable)

instance Accept AtomFeed where
    contentType _ = "application" // "atom+xml"

newtype FeedRendererException = FeedRendererException (Set Text)
    deriving Show

instance Exception FeedRendererException

instance MimeRender AtomFeed Feed where
    mimeRender :: Proxy AtomFeed -> Feed -> ByteString
    mimeRender _ val =
        encodeUtf8 . renderText def . toDocument . fromRight' . fromXMLElement
            $ xmlFeed val
      where
        toDocument v = Document
            { documentPrologue = Prologue
                { prologueBefore = []
                , prologueDoctype = Nothing
                , prologueAfter = []
                }
            , documentRoot = v
            , documentEpilogue = []
            }

        fromRight' (Right v) = v
        fromRight' (Left e) = throw $ FeedRendererException e

instance MimeUnrender AtomFeed Feed where
    mimeUnrender :: Proxy AtomFeed -> ByteString -> Either String Feed
    mimeUnrender _ bs =
        maybeToRight "Can't parse atom feed!" $ parseFeedSource bs

type RssApi = "atom" :> Get '[AtomFeed] Feed

rssApiHandler :: RestServer RssApi
rssApiHandler = do
    (feeds, lastModification) <- listFeeds
    context@Context{..} <- ask

    pure . feedFromAtom . fd context lastModification
        . toList $ fmap toEntry feeds

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
        { entryLinks = [nullLink $ showT url]
        , entrySummary = Just . HTMLString
        -- TODO: Use some HTML template language
            $ "<div><a href=\"" <> showT url <> ">\"<img src=\"" <> showT imgUrl
            <> "\"></div>"
        }
      where
        toEntry' :: Entry
        toEntry' = nullEntry (showT url)
            (TextString $ name <> " [Episode: " <> showT episodeNumber <> "]")
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

showT :: Show a => a -> Text
showT = pack . show
