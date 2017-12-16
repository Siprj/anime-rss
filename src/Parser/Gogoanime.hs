{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Data.ByteString.Lazy.Char8
import Data.Monoid
import Network.Wreq
import qualified Control.Lens as L
import Control.Arrow
import Text.HandsomeSoup
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlArrow

import Parser.Type


gogoanimeUrl :: String
gogoanimeUrl = "https://ww1.gogoanime.io/"

getEntrisFromFronPage :: String -> IO [AnimeEntry]
getEntrisFromFronPage url = do
    data' <- get url >>= (pure . Data.ByteString.Lazy.Char8.unpack . L.view responseBody)
    runX $ (parseHtml data') >>> css ".items" >>> css ".img" >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a) => a XmlTree AnimeEntry
    parseEntry = proc x -> do
        link <- getAttrValue "href" -< x
        title <- getAttrValue "title" -< x
        imageUrl <- (getChildren >>> hasName "img" >>> getAttrValue "src") -< x
        returnA -< AnimeEntry
            { url = url <> link
            , title
            , imageUrl
            }
