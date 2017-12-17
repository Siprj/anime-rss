module Main where

import Data.Proxy
import Network.Wai
import Servant.Server
import Network.Wai.Handler.Warp

import Parser.Gogoanime
import Rest

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
