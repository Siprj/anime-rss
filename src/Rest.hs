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
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader, ask)
import Data.ByteString.Lazy (ByteString)
import Data.Default(def)
import Data.Either (Either(Right, Left))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Proxy(Proxy)
import Data.Set (Set)
import Data.String (String)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text (Text, pack)
import Data.Time.Format (formatTime, rfc822DateFormat, defaultTimeLocale)
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
    ( EntryContent(HTMLContent)
    , Entry(entryContent, entryLinks)
    , TextContent(TextString)
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

import qualified Database.Model as DataModel
    (Feed(Feed, name, url, date, imgUrl))
import Database.Service (Database, listFeeds)


data Context = Context
    { baseUri :: URI
    , title :: Text
    }

type Handler' = Eff [Reader Context, Database, IO, Handler]
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
    feeds <- listFeeds
    context@Context{..}<- ask

    pure . feedFromAtom . fd context . toList $ fmap toEntry feeds

  where
    fd :: Context -> [Entry] -> Atom.Feed
    fd Context{..} entries = fd'
        { feedLinks = [nullLink $ showT baseUri]
        , feedEntries = entries
        }
      where
        fd' = nullFeed (showT baseUri) (TextString title) ""

    toEntry :: DataModel.Feed -> Entry
    toEntry DataModel.Feed{..} = toEntry'
        { entryLinks = [nullLink $ showT url]
        , entryContent = Just . HTMLContent
            $ "<p><img src=\"" <> showT imgUrl <> "\"></p> <p> New episode: "
            <> name <> "</p>"
        }
      where
        toEntry' :: Entry
        toEntry' = nullEntry (showT url) (TextString name)
            .  pack $ formatTime defaultTimeLocale rfc822DateFormat date

showT :: Show a => a -> Text
showT = pack . show
