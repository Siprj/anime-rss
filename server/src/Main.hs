{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import AnimeRss.Rest.Server (
  AuthSessionHandler,
  Context (Context),
  apiHander,
  authHandlerSession,
 )
import AnimeRss.Scraper.Service (runScraper)
import Configuration (
  Configuration (Configuration, databaseConnectionString, logEnpoint, traceEndpoint),
  getConfiguration,
 )
import Control.Applicative (pure)
import Control.Monad (forever, void)
import DBE (createConnectionPool, runDBE)
import Data.Either (either)
import Data.Function (($), (.))
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Monoid (mempty)
import Data.Pool (withResource)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (singleton)
import Effectful (MonadIO (liftIO), runEff)
import Effectful.Concurrent (forkIO, runConcurrent)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Effectful.Reader.Dynamic (runReader)
import Network.URI (URI)
import Network.URI.Static (staticURI)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setInstallShutdownHandler, setPort)
import Optics ((&), (.~))
import Otel.Client (OtelClientParameters (logEnpoint, traceEndpoint), defautOtelClientParameters, startOtelClient)
import Otel.Effect (runOtel, spanLink)
import Otel.Type (SpanKind (Internal), TraceData (TraceData))
import ResourceAttributes (resourceAttributes)
import Servant (Context (EmptyContext, (:.)), HasServer (hoistServerWithContext), serveWithContext, throwError)
import System.IO (IO)
import System.Posix.Signals (
  Handler (Catch),
  installHandler,
  sigINT,
  sigTERM,
 )

-- FIXME: This and other hardoced URLs need to be dynamic and need to be sotred
-- in the DB.
baseUrl :: URI
baseUrl = $$(staticURI "https://gogoanime.sk/")

main :: IO ()
main = do
  resourceAttributes' <- resourceAttributes
  Configuration {..} <- getConfiguration
  otelClient <- startOtelClient resourceAttributes' (defautOtelClientParameters & #logEnpoint .~ logEnpoint & #traceEndpoint .~ traceEndpoint)
  runEff . runConcurrent . runOtel otelClient (Just $ TraceData "Main" Internal mempty mempty) $ do
    dbPool <- createConnectionPool databaseConnectionString
    liftIO $ withResource dbPool migrateAll
    void . forkIO $ runScraper' dbPool
    restApp dbPool otelClient
  where
    restApp dbPool otelClient = do
      link <- spanLink
      let links = maybe mempty singleton link
      let cfg = authHandlerSession nat :. EmptyContext
          nat eff = do
            res <-
              liftIO . runEff . runErrorNoCallStack . runDBE dbPool . runOtel otelClient (Just $ TraceData "Warp" Internal mempty links) $
                runReader (Context baseUrl) eff
            either throwError pure res

      liftIO . runSettings settings . serveWithContext restAPI cfg $
        hoistServerWithContext restAPI (Proxy :: Proxy '[AuthSessionHandler]) nat apiHander
      where
        settings = setPort 8080 $ setInstallShutdownHandler shutdownHandler defaultSettings
        shutdownHandler closeSocket = do
          void $ installHandler sigTERM (Catch closeSocket) Nothing
          void $ installHandler sigINT (Catch closeSocket) Nothing

    restAPI :: Proxy Api
    restAPI = Proxy

    runScraper' dbPool =
      -- FIXME: This must catch exceptions and try to restart....
      --
      -- Time is is in milliseconds!!!
      -- This is approximately 15 minutes.
      forever . runDBE dbPool $ runScraper 1000000000
