{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

-- import DataModel.Service
--    ( DataModel
--    , createDataModelChannel
--    , runDataModel
--    , runDataModelEffect
--    )

import AnimeRss.DataModel.Migrations (migrateAll)
import AnimeRss.Rest.Api (Api)
import AnimeRss.Scraper.Service (runScraper)
import Control.Applicative (pure)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import DBE (createConnectionPool, runDBE)
import Data.Either (either)
import Data.Function (($), (.))
import Data.Pool (withResource)
import Data.Proxy (Proxy (Proxy))
import Effectful (MonadIO (liftIO), runEff)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.Reader.Dynamic (runReader)
import Network.URI (URI)
import Network.URI.Static (staticURI)
import Network.Wai.Handler.Warp (run)
import Servant (Context (EmptyContext, (:.)), HasServer (hoistServerWithContext), serveWithContext, throwError)
import System.IO (IO)
import AnimeRss.Rest.Server
    ( Context(Context),
      AuthSessionHandler,
      authHandlerSession,
      apiHander )

baseUrl :: URI
baseUrl = $$(staticURI "https://gogoanime.sk/")

main :: IO ()
main = do
  dbPool <- createConnectionPool "host=postgres dbname=anime-rss user=postgres"
  withResource dbPool migrateAll
  _ <- forkIO $ runScraper' dbPool
  restApp dbPool
  forever $ threadDelay 1000000000
  where
    restApp dbPool = do
      let natForAuth eff = do
            res <-
              liftIO . runEff . runErrorNoCallStack . runDBE dbPool $
                runReader (Context baseUrl) eff
            either throwError pure res
          cfg = authHandlerSession natForAuth :. EmptyContext
          nat eff = do
            res <-
              liftIO . runEff . runErrorNoCallStack . runDBE dbPool $
                runReader (Context baseUrl) eff
            either throwError pure res

      run 8080 . serveWithContext restAPI cfg $
        hoistServerWithContext restAPI (Proxy :: Proxy '[AuthSessionHandler]) nat apiHander

    restAPI :: Proxy Api
    restAPI = Proxy

    runScraper' dbPool =
      -- Time is is in milliseconds!!!
      -- This is approximately 15 minutes.
      forever . runEff . runDBE dbPool $ runScraper 1000000000
