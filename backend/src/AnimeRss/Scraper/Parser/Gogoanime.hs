{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module AnimeRss.Scraper.Parser.Gogoanime
    ( getEntrisFromFronPage
    , gogoanimeUrl
    )
  where

import Control.Applicative (pure)
import Control.Arrow.ArrowList (isA)
import Control.Arrow ((>>>), (|||), ArrowChoice, arr, returnA, zeroArrow)
import Optics (lensVL, view)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Either (Either (Left))
import Data.Functor ((<&>), fmap)
import Data.Function (($), (.), id)
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
import Data.List (break, reverse, stripPrefix, null)
import Data.Eq ((==))
import Data.Either.Combinators (maybeToRight)
import Control.Monad (when)
import AnimeRss.DataModel.Types (CreateEpisode(..))
import AnimeRss.Url


gogoanimeUrl :: URI
gogoanimeUrl = $$(staticURI "https://gogoanime.gr/")

-- TODO: Save how many episodes was parsed and try use this information to
-- determine that parser failed.
getEntrisFromFronPage :: URI -> IO [CreateEpisode]
getEntrisFromFronPage url = do
    data' <- get (show url) <&> (unpack . view (lensVL responseBody))
    runX $ parseHtml data' >>> css ".items" >>> css ".img"
        >>> deep (hasName "a" >>> parseEntry)
  where
    parseEntry :: (ArrowXml a, ArrowChoice a) => a XmlTree CreateEpisode
    parseEntry = proc x -> do
        linkStr <- getAttrValue "href" -< x
        link <- arr parseRelativeReference >>> isA isJust >>> arr fromJust -< linkStr
        (animeUrlStrin, number) <- arr episodeNumberAndLinkParser >>> zeroArrow ||| arr id -< linkStr
        title <- getAttrValue "title" >>> arr pack -< x
        imageUrl <- getChildren >>> hasName "img" >>> getAttrValue "src"
            >>> arr parseURI >>> isA isJust >>> arr fromJust -< x
        animeUrl <- arr parseRelativeReference >>> isA isJust >>> arr fromJust -< animeUrlStrin
        returnA -< CreateEpisode
            { url = Url $ link `relativeTo` url
            , title
            , imageUrl = Url imageUrl
            , number = pack number
            , animeUrl = Url $ animeUrl `relativeTo` url
            }

episodeNumberAndLinkParser :: String -> Either String (String, String)
episodeNumberAndLinkParser input = do
  let (rEpisodeNumber, rest) = break (== '-') $ reverse input
      episodeNumber = reverse rEpisodeNumber
  link <- fmap reverse . maybeToRight "`-episode-` is not present in the link" $ stripPrefix (reverse "-episode-") rest
  when (null episodeNumber) $ Left "episode number is empty"
  pure ("category/" <> link, episodeNumber)
