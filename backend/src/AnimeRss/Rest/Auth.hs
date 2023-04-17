{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- It's almost a copy of Servant.Auth.Server.Internal.AddSetCookie module
-- from the servant-auth-server library.

module AnimeRss.Rest.Auth (
  module AnimeRss.Rest.Cookies,
  mkAPIAuthHandler,
  APIAuthHandler (..),
  APIAuthProtect,
  APIAuthServerData,
  APIAuthServerDataCookies,
) where

-- import Control.Monad.Reader

import AnimeRss.Rest.Cookies
import Control.Applicative (pure)
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.Either
import Data.Function ((.))
import Data.Functor
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics
import Network.Wai (Request)
import Servant ((:>))
import Servant.API.ContentTypes
import Servant.API.Verbs
import Servant.Server.Internal

type family APIAuthServerData a :: Type

type family APIAuthServerDataCookies (a :: k) :: Nat

-- | 'cookieNumber' references the number of extra cookies that we want to
-- define for a request.
newtype APIAuthHandler r user cookieNumber = APIAuthHandler
  {getAPIAuthHandler :: r -> Handler (user, SetCookieList cookieNumber)}
  deriving stock (Generic)

mkAPIAuthHandler ::
  (r -> Handler (user, SetCookieList cookieNumber)) ->
  APIAuthHandler r user cookieNumber
mkAPIAuthHandler = APIAuthHandler

data APIAuthProtect (tag :: k)

-- | known orphan instance
instance
  ( HasServer api context
  , cookieNumber ~ APIAuthServerDataCookies (APIAuthProtect tag)
  , apiWithCookies ~ AddSetCookiesApi cookieNumber api
  , HasServer apiWithCookies context
  , HasContextEntry context (APIAuthHandler Request (APIAuthServerData (APIAuthProtect tag)) cookieNumber)
  , AddSetCookies cookieNumber (ServerT api Handler) (ServerT apiWithCookies Handler)
  ) =>
  HasServer (APIAuthProtect tag :> api) context
  where
  type
    ServerT (APIAuthProtect tag :> api) m =
      APIAuthServerData (APIAuthProtect tag) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

  route _ context subserver =
    route
      (Proxy @apiWithCookies)
      context
      (fmap setCookies subserver `addAuthCheck` withRequest authCheck)
    where
      authHandler ::
        Request ->
        Handler (APIAuthServerData (APIAuthProtect tag), SetCookieList cookieNumber)
      authHandler = getAPIAuthHandler (getContextEntry context)

      authCheck ::
        Request ->
        DelayedIO
          (APIAuthServerData (APIAuthProtect tag), SetCookieList cookieNumber)
      authCheck = either delayedFailFatal pure <=< liftIO . runHandler . authHandler

      setCookies ::
        (APIAuthServerData (APIAuthProtect tag) -> ServerT api Handler) ->
        (APIAuthServerData (APIAuthProtect tag), SetCookieList cookieNumber) ->
        ServerT apiWithCookies Handler
      setCookies fn (authResult, cookies) = addSetCookies cookies (fn authResult)

-- due to https://github.com/haskell-servant/servant/issues/1267
type instance
  AddSetCookieApi (NoContentVerb method) =
    Verb method 204 '[JSON] (AddSetCookieApiVerb NoContent)
