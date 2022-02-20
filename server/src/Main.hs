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

import Control.Concurrent (forkIO)
import Control.Monad.Freer.Internal (Eff)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer (runM, runNat)
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Proxy (Proxy(Proxy))
import Network.URI (URI)
import Network.Wai.Handler.Warp (run)
import Rest.Server (Context(Context), apiHander)
import Rest.Api (Api)
import Servant.Server (Handler, Context ((:.), EmptyContext), serveWithContext, HasServer (hoistServerWithContext))
import System.IO (IO)

import Control.Monad.Freer.Service (ServiceChannel)
import DataModel.Service
    ( DataModel
    , createDataModelChannel
    , runDataModel
    , runDataModelEffect
    )
import Network.URI.Static (staticURI)
import Scraper.Service (runScraper)
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey, CookieSettings (cookieIsSecure), JWTSettings, IsSecure (NotSecure))


baseUrl :: URI
baseUrl = $$(staticURI "https://gogoanime.io/")

main :: IO ()
main = do
    databaseChan <- createDataModelChannel
    _ <- forkIO $ runDataModel databaseChan

    _ <- forkIO $ runScraper' databaseChan
    restApp databaseChan
  where

    restApp :: ServiceChannel DataModel -> IO ()
    restApp databaseChan = do
        myKey <- generateKey
        let cookiesSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
            jwtCfg = defaultJWTSettings myKey
            cfg = cookiesSettings :. jwtCfg :. EmptyContext
            nat eff =
                runM . intLiftIO . runDataModelEffect databaseChan
                $ runReader eff (Context baseUrl cookiesSettings jwtCfg )

        run 8082 .  serveWithContext restAPI cfg $
                hoistServerWithContext restAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) nat apiHander

    restAPI :: Proxy Api
    restAPI = Proxy


    runScraper' databaseChan =
        -- Time is is in milliseconds!!!
        -- This is approximately 15 minutes.
        runM . runDataModelEffect databaseChan $ runScraper 1000000000

intLiftIO :: Eff '[IO, Handler] r -> Eff '[Handler] r
intLiftIO = runNat (liftIO :: IO a -> Handler a)

