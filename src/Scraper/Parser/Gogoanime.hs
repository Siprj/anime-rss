{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Scraper.Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Control.Applicative ((<|>), pure)
import Control.Arrow.ArrowList (isA)
import Control.Arrow ((>>>), (|||), ArrowChoice, arr, returnA, zeroArrow)
import Optics (lensVL, view)
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
import Data.Semigroup ((<>))
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
import Core.Type.EpisodeEntry
    (animeUrl, EpisodeEntry(EpisodeEntry, imageUrl, title, url, episodeNumber))


gogoanimeUrl :: URI
gogoanimeUrl = $$(staticURI "https://gogoanime.io/")

-- TODO: Save how many episodes was parsed and try use this information to
-- determine that parser failed.
getEntrisFromFronPage :: URI -> IO [EpisodeEntry]
getEntrisFromFronPage url = do
    data' <- get (show url) >>= (pure . unpack . view (lensVL responseBody))
    runX $ (parseHtml data') >>> css ".items" >>> css ".img"
        >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a, ArrowChoice a) => a XmlTree EpisodeEntry
    parseEntry = proc x -> do
        linkStr <- getAttrValue "href" -< x
        link <- arr parseRelativeReference >>> isA isJust >>> arr fromJust -< linkStr
        episodeNumber <- arr episodeNumberParser >>> zeroArrow ||| arr id -< linkStr
        title <- getAttrValue "title" >>> arr pack -< x
        imageUrl <- getChildren >>> hasName "img" >>> getAttrValue "src"
            >>> arr parseURI >>> isA isJust >>> arr fromJust -< x
        animeUrl <- arr parseRelativeReference >>> isA isJust
            >>> arr fromJust -< ("category/" <> linkStr)
        returnA -< EpisodeEntry
            { url = link `relativeTo` url
            , title
            , imageUrl
            , episodeNumber
            , animeUrl
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
