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
import Control.Monad.Freer (runM, send, runNat)
import Control.Monad.Freer.Internal (Eff(E, Val))
import Control.Monad.Freer.Reader (runReader)
import Data.Acid (openLocalState)
import Database.Model (defaultDataModel)
import Data.Function (($), (.))
import Database.Service
    ( Database
    , createDatabaseChannel
    , runDatabase
    , runDatabaseEffect
    )
import Data.Proxy (Proxy(Proxy))
import Network.URI (URI)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Rest (RssApi, Context(Context, baseUri, title), rssApiHandler)
import Servant.Server (Handler, serve, hoistServer)
import Control.Monad.IO.Class (liftIO)
import System.IO (IO, print)

import Control.Monad.Freer.Service (ServiceChannel)
import Database.Service (Database)
import Network.URI.Static (staticURI)

baseUrl :: URI
baseUrl = $$(staticURI "https://ww1.gogoanime.io/")

main :: IO ()
main = do
    dataModel <- openLocalState defaultDataModel
    databaseChan <- createDatabaseChannel
    forkIO $ runDatabase dataModel databaseChan

    getEntrisFromFronPage gogoanimeUrl >>= print
    run 8081 (restApp databaseChan)
  where

    -- 'serve' comes from servant and hands you a WAI Application,
    -- which you can think of as an "abstract" web application,
    -- not yet a webserver.
    restApp :: ServiceChannel Database -> Application
    restApp databaseChan = serve restAPI (mainServer databaseChan)

    restAPI :: Proxy RssApi
    restAPI = Proxy

    mainServer databaseChan =
        hoistServer restAPI (nat databaseChan) rssApiHandler
    nat databaseChan eff =
        runM . intLiftIO . runDatabaseEffect databaseChan
        $ runReader eff (Context baseUrl "pokus")

intLiftIO :: Eff '[IO, Handler] r -> Eff '[Handler] r
intLiftIO = runNat (liftIO :: IO a -> Handler a)
