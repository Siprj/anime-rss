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
import Control.Monad ((>>=))
import Control.Monad.Freer.Internal (Eff(E, Val))
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Freer (runM, send, runNat)
import Control.Monad.IO.Class (liftIO)
import Data.Function (($), (.))
import Data.Proxy (Proxy(Proxy))
import Network.URI (URI)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Rest.Server (Context(Context, baseUri), apiHander)
import Rest.Api (Api)
import Servant.Server (Handler, serve, hoistServer, Context ((:.), EmptyContext), serveWithContext, HasServer (hoistServerWithContext))
import qualified Servant.Server as Servant (Context)
import System.IO (IO, print)

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
    forkIO $ runDataModel databaseChan

    forkIO $ runScraper' databaseChan
    restApp databaseChan
  where

    restApp :: ServiceChannel DataModel -> IO ()
    restApp databaseChan = do
        myKey <- generateKey
        let cookiesSettings = defaultCookieSettings { cookieIsSecure = NotSecure }
            jwtCfg = defaultJWTSettings myKey
            cfg = cookiesSettings :. jwtCfg :. EmptyContext
            nat databaseChan eff =
                runM . intLiftIO . runDataModelEffect databaseChan
                $ runReader eff (Context baseUrl cookiesSettings jwtCfg )

        run 8081 .  serveWithContext restAPI cfg $
                hoistServerWithContext restAPI (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nat databaseChan) apiHander

    restAPI :: Proxy Api
    restAPI = Proxy


    runScraper' databaseChan =
        -- Time is is in milliseconds!!!
        -- This is approximately 15 minutes.
        runM . runDataModelEffect databaseChan $ runScraper 1000000000

intLiftIO :: Eff '[IO, Handler] r -> Eff '[Handler] r
intLiftIO = runNat (liftIO :: IO a -> Handler a)

