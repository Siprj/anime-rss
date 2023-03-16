{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AnimeRss.Scraper.Service
    ( runScraper
    )
  where

import Control.Monad (mapM_)
import Control.Concurrent (threadDelay)
import Data.Function (($), (.))
import Data.Int (Int)
import System.IO (putStrLn)

import AnimeRss.Scraper.Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Effectful (Eff, (:>), liftIO, IOE)
import DBE (PostgreSql, withTransaction)
import AnimeRss.DataModel.Queries (insertEpisode)


runScraper :: (PostgreSql :> es, IOE :> es) => Int -> Eff es ()
runScraper time = do
    -- TODO: Add exception handling...
    -- TODO: Use logging instead of this print.
    liftIO $ putStrLn "running scraper"
    entries <- liftIO $ getEntrisFromFronPage gogoanimeUrl
    liftIO $ putStrLn "data from gogoanime received"
    -- TODO: Test how long it took to insert data into database and log it.
    mapM_ (withTransaction . insertEpisode) entries
    liftIO $ putStrLn "scrapper finished"
    liftIO $ threadDelay time
