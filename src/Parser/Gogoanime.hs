{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Data.ByteString.Lazy.Char8
import Data.Maybe (isJust, fromJust)
import Data.Monoid
import Network.Wreq (get, responseBody)
import Network.URI (URI, parseRelativeReference, relativeTo)
import qualified Control.Lens as L
import Control.Arrow
import Control.Arrow.ArrowList
import Text.HandsomeSoup
import Text.Show (show)
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlArrow

import Parser.Type
import Network.URI.Static (staticURI)


gogoanimeUrl :: URI
gogoanimeUrl = $$(staticURI "https://ww1.gogoanime.io/")

getEntrisFromFronPage :: URI -> IO [AnimeEntry]
getEntrisFromFronPage url = do
    data' <- get (show url) >>= (pure . Data.ByteString.Lazy.Char8.unpack . L.view responseBody)
    runX $ (parseHtml data') >>> css ".items" >>> css ".img" >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a) => a XmlTree AnimeEntry
    parseEntry = proc x -> do
        link <- (getAttrValue "href" >>> arr parseRelativeReference >>> isA isJust >>> arr fromJust) -< x
        title <- getAttrValue "title" -< x
        imageUrl <- (getChildren >>> hasName "img" >>> getAttrValue "src") -< x
        returnA -< AnimeEntry
            { url = link `relativeTo` url
            , title
            , imageUrl
            }
