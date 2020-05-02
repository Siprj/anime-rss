{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Rest (RssApi, Context(Context, baseUri, title), rssApiHandler)
import Servant.Server (Handler, serve, hoistServer)
import System.IO (IO, print)

import Control.Monad.Freer.Service (ServiceChannel)
import DataModel.Service
    ( DataModel
    , createDataModelChannel
    , runDataModel
    , runDataModelEffect
    )
import Network.URI.Static (staticURI)
import Scraper.Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Scraper.Service (runScraper)


baseUrl :: URI
baseUrl = $$(staticURI "https://gogoanime.io/")

main :: IO ()
main = do
    databaseChan <- createDataModelChannel
    forkIO $ runDataModel databaseChan

    getEntrisFromFronPage gogoanimeUrl >>= print
    forkIO $ runScraper' databaseChan
    run 8081 (restApp databaseChan)
  where

    -- 'serve' comes from servant and hands you a WAI Application,
    -- which you can think of as an "abstract" web application,
    -- not yet a webserver.
    restApp :: ServiceChannel DataModel -> Application
    restApp databaseChan = serve restAPI (mainServer databaseChan)

    restAPI :: Proxy RssApi
    restAPI = Proxy

    mainServer databaseChan =
        hoistServer restAPI (nat databaseChan) rssApiHandler
    nat databaseChan eff =
        runM . intLiftIO . runDataModelEffect databaseChan
        $ runReader eff (Context baseUrl "pokus")

    runScraper' databaseChan =
        runM . runDataModelEffect databaseChan $ runScraper 1000000000

intLiftIO :: Eff '[IO, Handler] r -> Eff '[Handler] r
intLiftIO = runNat (liftIO :: IO a -> Handler a)
