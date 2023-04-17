{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AnimeRss.Rest.SetCookies (
  SetCookies,
  SetCookiesNumber,
  CookiesSetter,
  mkCookiesSetter,
) where

import AnimeRss.Rest.Cookies
import Data.Functor
import Data.Proxy (Proxy (..))
import GHC.Generics
import Network.Wai (Request)
import Servant.API
import Servant.Server
import Servant.Server.Internal.Delayed (passToServer)

data SetCookies (tag :: k)

type family SetCookiesNumber (a :: k) :: Nat

newtype CookiesSetter req cookieNumber = CookiesSetter
  {getSetCookiesList :: req -> SetCookieList cookieNumber}
  deriving stock (Generic)

mkCookiesSetter :: (req -> SetCookieList cookieNumber) -> CookiesSetter req cookieNumber
mkCookiesSetter = CookiesSetter

-- | known orphan instance
instance
  ( HasServer api context
  , cookieNumber ~ SetCookiesNumber (SetCookies tag)
  , apiWithCookies ~ AddSetCookiesApi cookieNumber api
  , HasServer apiWithCookies context
  , HasContextEntry context (CookiesSetter Request cookieNumber)
  , AddSetCookies cookieNumber (ServerT api Handler) (ServerT apiWithCookies Handler)
  ) =>
  HasServer (SetCookies tag :> api) context
  where
  type ServerT (SetCookies tag :> api) m = ServerT api m

  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

  route _ context subserver =
    route
      (Proxy @apiWithCookies)
      context
      (fmap setCookies subserver `passToServer` getCookies)
    where
      getCookies :: Request -> SetCookieList cookieNumber
      getCookies = getSetCookiesList (getContextEntry context)

      setCookies ::
        ServerT api Handler ->
        SetCookieList cookieNumber ->
        ServerT apiWithCookies Handler
      setCookies fn cookies = addSetCookies cookies fn
