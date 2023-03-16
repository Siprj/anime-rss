{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main
    (main)
  where

import Control.Concurrent (forkIO, threadDelay)
import Data.Function (($), (.))
import AnimeRss.Rest.Server (Context(Context), apiHander)
import AnimeRss.Rest.Api (Api)
import System.IO (IO)
import Effectful (runEff, MonadIO (liftIO))

--import DataModel.Service
--    ( DataModel
--    , createDataModelChannel
--    , runDataModel
--    , runDataModelEffect
--    )
import AnimeRss.Scraper.Service (runScraper)
import DBE (runDBE, createConnectionPool)
import Control.Monad (forever)
import Network.URI (URI)
import Servant.Auth.Server (CookieSettings(cookieIsSecure), JWTSettings, generateKey, IsSecure (NotSecure), defaultCookieSettings, defaultJWTSettings)
import Data.Proxy (Proxy(Proxy))
import Network.URI.Static ( staticURI )
import Servant (Context((:.), EmptyContext), throwError, HasServer (hoistServerWithContext), serveWithContext)
import Effectful.Reader.Dynamic ( runReader )
import Data.Either ( either )
import Network.Wai.Handler.Warp (run)
import AnimeRss.DataModel.Migrations ( migrateAll )
import Control.Applicative (pure)
import Effectful.Error.Dynamic (runErrorNoCallStack)
import Data.Pool (withResource)


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
        let cookiesSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
            jwtCfg = defaultJWTSettings myKey
            cfg = cookiesSettings :. jwtCfg :. EmptyContext
            nat eff = do
                res <- liftIO . runEff . runErrorNoCallStack . runDBE dbPool
                  $ runReader (Context baseUrl cookiesSettings jwtCfg ) eff
                either throwError pure res

        run 8080 .  serveWithContext restAPI cfg $
                hoistServerWithContext restAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) nat apiHander

    restAPI :: Proxy Api
    restAPI = Proxy


    runScraper' dbPool =
        -- Time is is in milliseconds!!!
        -- This is approximately 15 minutes.
        forever . runEff . runDBE dbPool $ runScraper 1000000000
