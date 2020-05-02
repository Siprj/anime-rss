{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Scraper.Service
    ( runScraper
    )
  where

import Control.Monad (mapM_)
import Control.Monad.Freer (Eff, send)
import Control.Concurrent (threadDelay)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Time.Clock (getCurrentTime)
import System.IO (IO, putStrLn)

import DataModel.Persistent
    ( Feed
        ( Feed
        , feedName
        , feedUrl
        , feedImgUrl
        , feedEpisodeNumber
        , feedDate
        )
    )
import DataModel.Service (DataModel, addFeedIfUnique)
import Scraper.Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Scraper.Parser.Type
    (AnimeEntry(AnimeEntry, title, url, imageUrl, episodeNumber))


runScraper :: Int -> Eff [DataModel, IO] ()
runScraper time = do
    -- TODO Use logging instead of this print.
    send $ putStrLn "running scraper"
    entries <- send $ getEntrisFromFronPage gogoanimeUrl
    currentTime <- send $ getCurrentTime
    mapM_ (addFeedIfUnique . toSetFeed currentTime) entries
    send $ threadDelay time
    runScraper time
  where
    toSetFeed currentTime AnimeEntry{..} = Feed
        { feedName = title
        , feedUrl = url
        , feedImgUrl = imageUrl
        , feedEpisodeNumber = episodeNumber
        , feedDate = currentTime
        }
