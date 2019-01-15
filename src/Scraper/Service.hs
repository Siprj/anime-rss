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
import System.IO (IO)

import DataModel.Type.Feed
    ( Feed'
        ( Feed'
        , _name
        , _url
        , _imgUrl
        , _episodeNumber
        , _date
        )
    )
import DataModel.Service (DataModel, addFeedIfUnique)
import Scraper.Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Scraper.Parser.Type
    (AnimeEntry(AnimeEntry, title, url, imageUrl, episodeNumber))


runScraper :: Int -> Eff [DataModel, IO] ()
runScraper time = do
    entries <- send $ getEntrisFromFronPage gogoanimeUrl
    mapM_ (addFeedIfUnique . toSetFeed) entries
    send $ threadDelay time
    runScraper time
  where
    toSetFeed AnimeEntry{..} = Feed'
        { _name = title
        , _url = url
        , _imgUrl = imageUrl
        , _episodeNumber = episodeNumber
        , _date = ()
        }
