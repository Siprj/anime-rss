{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Control.Applicative (pure)
import Control.Arrow.ArrowList (isA)
import Control.Arrow ((>>>), arr, returnA)
import Control.Lens (view)
import Control.Monad ((>>=))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Function (($), (.))
import Data.Maybe (fromJust, isJust)
import Network.URI (URI, parseRelativeReference, relativeTo)
import Network.Wreq (get, responseBody)
import System.IO (IO)
import Text.HandsomeSoup (css, parseHtml)
import Text.Show (show)
import Text.XML.HXT.Core
    ( ArrowXml
    , XmlTree
    , deep
    , getAttrValue
    , getChildren
    , hasName
    , runX
    )

import Network.URI.Static (staticURI)
import Parser.Type (AnimeEntry(AnimeEntry, imageUrl, title, url))


gogoanimeUrl :: URI
gogoanimeUrl = $$(staticURI "https://ww1.gogoanime.io/")

getEntrisFromFronPage :: URI -> IO [AnimeEntry]
getEntrisFromFronPage url = do
    data' <- get (show url) >>= (pure . unpack . view responseBody)
    runX $ (parseHtml data') >>> css ".items" >>> css ".img"
        >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a) => a XmlTree AnimeEntry
    parseEntry = proc x -> do
        link <- (getAttrValue "href" >>> arr parseRelativeReference
            >>> isA isJust >>> arr fromJust) -< x
        title <- getAttrValue "title" -< x
        imageUrl <- (getChildren >>> hasName "img" >>> getAttrValue "src") -< x
        returnA -< AnimeEntry
            { url = link `relativeTo` url
            , title
            , imageUrl
            }
