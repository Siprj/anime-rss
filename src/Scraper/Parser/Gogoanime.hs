{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scraper.Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Control.Applicative ((<|>), pure)
import Control.Arrow.ArrowList (isA)
import Control.Arrow ((>>>), (|||), ArrowChoice, arr, returnA, zeroArrow)
import Control.Lens (view)
import Control.Monad ((>>=), void)
import Data.Attoparsec.ByteString.Char8
    ( Parser
    , anyChar
    , char
    , decimal
    , endOfInput
    , manyTill
    , parseOnly
    )
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.Either (Either)
import Data.Function (($), (.), id)
import Data.Int (Int)
import Data.Maybe (fromJust, isJust)
import Data.String (String)
import Data.Text (pack)
import Network.URI (URI, parseRelativeReference, relativeTo, parseURI)
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
import Scraper.Parser.Type
    (AnimeEntry(AnimeEntry, imageUrl, title, url, episodeNumber))


gogoanimeUrl :: URI
gogoanimeUrl = $$(staticURI "https://ww1.gogoanime.io/")

getEntrisFromFronPage :: URI -> IO [AnimeEntry]
getEntrisFromFronPage url = do
    data' <- get (show url) >>= (pure . unpack . view responseBody)
    runX $ (parseHtml data') >>> css ".items" >>> css ".img"
        >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a, ArrowChoice a) => a XmlTree AnimeEntry
    parseEntry = proc x -> do
        linkStr <- getAttrValue "href" -< x
        link <- arr parseRelativeReference >>> isA isJust >>> arr fromJust -< linkStr
        episodeNumber <- arr episodeNumberParser >>> zeroArrow ||| arr id -< linkStr
        title <- getAttrValue "title" >>> arr pack -< x
        imageUrl <- getChildren >>> hasName "img" >>> getAttrValue "src"
            >>> arr parseURI >>> isA isJust >>> arr fromJust -< x
        returnA -< AnimeEntry
            { url = link `relativeTo` url
            , title
            , imageUrl
            , episodeNumber
            }

episodeNumberParser :: String -> Either String Int
episodeNumberParser = parseOnly episodeNumberParser' . ByteString.pack
  where
    episodeNumberParser' :: Parser Int
    episodeNumberParser' = do
        void $ manyTill anyChar (char '-')
        value <|> episodeNumberParser'
      where
        value = do
            v <- decimal
            endOfInput
            pure v
