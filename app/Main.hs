{-# LANGUAGE NoImplicitPrelude #-}

module Main
    (main)
  where

import Control.Monad ((>>=))
import Data.Function (($))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.IO (IO, print)

import Parser.Gogoanime (getEntrisFromFronPage, gogoanimeUrl)
import Rest (RssApi, rssApiHandler)


userAPI :: Proxy RssApi
userAPI = Proxy

main :: IO ()
main = do
    getEntrisFromFronPage gogoanimeUrl >>= print
    run 8081 app1

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI $ rssApiHandler gogoanimeUrl
