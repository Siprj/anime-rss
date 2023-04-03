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
import AnimeRss.Rest.Server (Context (Context), apiHander)
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
import Servant.Auth.Server (CookieSettings (cookieIsSecure), IsSecure (NotSecure), JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import System.IO (IO)

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
      myKey <- generateKey
      let cookiesSettings = defaultCookieSettings {cookieIsSecure = NotSecure}
          jwtCfg = defaultJWTSettings myKey
          cfg = cookiesSettings :. jwtCfg :. EmptyContext
          nat eff = do
            res <-
              liftIO . runEff . runErrorNoCallStack . runDBE dbPool $
                runReader (Context baseUrl cookiesSettings jwtCfg) eff
            either throwError pure res

      run 8080 . serveWithContext restAPI cfg $
        hoistServerWithContext restAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) nat apiHander

    restAPI :: Proxy Api
    restAPI = Proxy

    runScraper' dbPool =
      -- Time is is in milliseconds!!!
      -- This is approximately 15 minutes.
      forever . runEff . runDBE dbPool $ runScraper 1000000000
