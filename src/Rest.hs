{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rest
    ( RssApi
    , rssApiHandler
    )
  where

import Control.Monad.IO.Class
import Data.Default(def)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid((<>))
import Data.ByteString.Lazy.Char8(pack)
import Data.Typeable (Typeable)
import Data.Text (Text, unpack)
import Network.HTTP.Media ((//), (/:))
import Network.URI (URI)
import Servant.API.ContentTypes
    ( Accept(contentType)
    , MimeRender(mimeRender)
    , MimeUnrender(mimeUnrender)
    )
import Text.XML.Light.Output (showElement)
import Text.XML.Light.Input (parseXMLDoc)
import Servant.API ((:>), Get)
import Servant.Server (Server)
import Text.Atom.Feed
    ( Feed(feedLinks, feedEntries)
    , Entry(entryAuthors, entryContent, entryLinks)
    , Person(personName)
    , TextContent(TextString)
    , EntryContent(HTMLContent)
    , nullFeed
    , nullEntry
    , nullLink
    , nullPerson
    )
import Text.Atom.Feed.Export (xmlFeed)
import Text.Atom.Feed.Import (elementFeed)
import Text.Show (show)


xmlVersion :: String
xmlVersion = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight _ (Just a) = Right a
maybeToRight err Nothing = Left err

data AtomFeed
  deriving (Typeable)

instance Accept AtomFeed where
    contentType _ = "application" // "atom+xml"

instance MimeRender AtomFeed Feed where
    mimeRender _ val =
        pack $ xmlVersion <> showElement (xmlFeed val)

instance MimeUnrender AtomFeed Feed where
    mimeUnrender _ bs =
        maybeToRight "Can't parse atom feed!" $ parseXMLDoc bs >>= elementFeed


type RssApi = "atom" :> Get '[AtomFeed] Feed

rssApiHandler :: URI -> Server RssApi
rssApiHandler baseUrl = do
    return $ fd {feedEntries = fmap toEntry posts, feedLinks = [nullLink "http://example.com/"]}
  where
    fd :: Feed
    fd = nullFeed
        (show baseUrl) -- ID
        (TextString "Example Website") -- Title
        "" -- Last updated

    toEntry :: (String, Text) -> Entry
    toEntry (url, content) = toEntry'
        { entryAuthors = [nullPerson {personName = "Pako"}]
        , entryLinks = [nullLink url]
        , entryContent = Just (HTMLContent $ unpack content)
        }
      where
        toEntry' = nullEntry
            url -- The ID field. Must be a link to validate.
            (TextString $ unpack content) -- Title "")
            ""

    posts =
        [ ("AHOJ", "asdfasfasfd")
        , ("BLABLA", "qqqqqqqqqqqqqqqqqqqqqqq")
        ]
