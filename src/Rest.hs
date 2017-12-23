{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Rest
    ( RssApi
    , rssApiHandler
    )
  where

import Control.Exception (Exception, throw)
import Control.Monad.IO.Class
import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Default(def)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.String (String)
import Data.Set (Set)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid((<>))
import Data.ByteString.Lazy.Char8(pack)
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text.Lazy as LZT (Text)
import qualified Data.Text.Lazy.Encoding as LZT (encodeUtf8)
import qualified Data.Text as Text (pack)
import Network.HTTP.Media ((//), (/:))
import Network.URI (URI)
import Servant.API.ContentTypes
    ( Accept(contentType)
    , MimeRender(mimeRender)
    , MimeUnrender(mimeUnrender)
    )
import Text.XML.Light.Output (showElement)
import Servant.API ((:>), Get)
import Servant.Server (Server)
import Text.Atom.Feed
    ( EntryContent(HTMLContent)
    , Entry(entryAuthors, entryContent, entryLinks)
    , Person(personName)
    , TextContent(TextString)
    , feedEntries
    , feedLinks
    , nullEntry
    , nullFeed
    , nullLink
    , nullPerson
    )
import qualified Text.Atom.Feed as Atom (Feed)
import Text.Atom.Feed.Import (elementFeed)
import Text.Feed.Export (xmlFeed)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)
import Text.Feed.Constructor (feedFromAtom)
import Text.XML
    ( Document(Document, documentPrologue, documentRoot, documentEpilogue)
    , Prologue(Prologue, prologueBefore, prologueDoctype, prologueAfter)
    , Miscellaneous(MiscComment)
    , fromXMLElement
    , renderText
    )
import Text.Show (Show, show)


maybeToRight :: e -> Maybe a -> Either e a
maybeToRight _ (Just a) = Right a
maybeToRight err Nothing = Left err

data AtomFeed
  deriving (Typeable)

instance Accept AtomFeed where
    contentType _ = "application" // "atom+xml"

data FeedRendererException = FeedRendererException (Set Text)
    deriving Show

instance Exception FeedRendererException

instance MimeRender AtomFeed Feed where
    mimeRender _ val =
        LZT.encodeUtf8 . renderText def . toDocument $ fromRight' $ fromXMLElement $ xmlFeed val
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
    mimeUnrender _ bs =
        maybeToRight "Can't parse atom feed!" $ parseFeedSource bs

type RssApi = "atom" :> Get '[AtomFeed] Feed

rssApiHandler :: URI -> Server RssApi
rssApiHandler baseUrl = do
    pure $ feedFromAtom $ fd
        { feedEntries = fmap toEntry posts
        , feedLinks = [nullLink "http://example.com/"]
        }
  where
    fd :: Atom.Feed
    fd = nullFeed
        (Text.pack $ show baseUrl) -- ID
        (TextString "Example Website") -- Title
        "" -- Last updated

    toEntry :: (Text, Text) -> Entry
    toEntry (url, content) = toEntry'
        { entryAuthors = [nullPerson {personName = "Pako"}]
        , entryLinks = [nullLink url]
        , entryContent = Just (HTMLContent content)
        }
      where
        toEntry' = nullEntry
            url -- The ID field. Must be a link to validate.
            (TextString content) -- Title "")
            ""

    posts =
        [ ("AHOJ", "asdfasfasfd")
        , ("BLABLA", "qqqqqqqqqqqqqqqqqqqqqqq")
        ]
