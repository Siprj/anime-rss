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
import Data.Function (($))
import Data.Int (Int)
import System.IO (IO, putStrLn)

import DataModel.Service (DataModel, addEpisodeEntryifUnique)
import Scraper.Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)


runScraper :: Int -> Eff [DataModel, IO] ()
runScraper time = do
    -- TODO Use logging instead of this print.
    send $ putStrLn "running scraper"
    -- TODO: Collect errors into EGK.
    entries <- send $ getEntrisFromFronPage gogoanimeUrl
    -- TODO: Test how long it took to insert data into database and log it.
    mapM_ addEpisodeEntryifUnique entries
    send $ threadDelay time
    runScraper time
