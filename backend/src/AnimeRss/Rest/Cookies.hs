{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- It's almost a copy of Servant.Auth.Server.Internal.AddSetCookie module
-- from the servant-auth-server library.
module AnimeRss.Rest.Cookies (Nat (..), ZeroCookies, OneCookie, TwoCookies, SetCookieList (..), AddSetCookiesApi, AddSetCookies, addSetCookies, AddSetCookieApi, AddSetCookieApiVerb) where

import Control.Applicative ((<$>))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Function (($), (.))
import Data.Functor (Functor)
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Network.HTTP.Types qualified as HTTP
import Network.Wai (mapResponseHeaders)
import Relude (identity, (<>))
import Servant (
  AddHeader,
  Application,
  GServantProduct,
  HasServer (ServerT),
  Header,
  Headers,
  NamedRoutes,
  Raw,
  Stream,
  Tagged (..),
  ToServant,
  ToServantApi,
  Verb,
  addHeader,
  noHeader,
  toServant,
  type (:<|>) (..),
  type (:>),
 )
import Servant.API.Generic (Generic (..))
import Servant.Server.Generic (AsServerT)
import Web.Cookie (SetCookie, renderSetCookie)

-- What are we doing here? Well, the idea is to add headers to the response,
-- but the headers come from the authentication check. In order to do that, we
-- tweak a little the general theme of recursing down the API tree; this time,
-- we recurse down a variation of it that adds headers to all the endpoints.
-- This involves the usual type-level checks.
--
-- TODO: If the endpoints already have headers, this will not work as is.

data Nat = Z | S Nat

type ZeroCookies = 'Z

type OneCookie = 'S 'Z

type TwoCookies = 'S ('S 'Z)

type family AddSetCookiesApi (n :: Nat) a where
  AddSetCookiesApi 'Z a = a
  AddSetCookiesApi ('S 'Z) a = AddSetCookieApi a
  AddSetCookiesApi ('S n) a = AddSetCookiesApi n (AddSetCookieApi a)

type family AddSetCookieApiVerb a where
  AddSetCookieApiVerb (Headers ls a) = Headers (Header "Set-Cookie" SetCookie ': ls) a
  AddSetCookieApiVerb a = Headers '[Header "Set-Cookie" SetCookie] a

type family AddSetCookieApi a :: Type

type instance AddSetCookieApi (a :> b) = a :> AddSetCookieApi b

type instance AddSetCookieApi (a :<|> b) = AddSetCookieApi a :<|> AddSetCookieApi b

type instance AddSetCookieApi (NamedRoutes api) = AddSetCookieApi (ToServantApi api)

type instance
  AddSetCookieApi (Verb method stat ctyps a) =
    Verb
      method
      stat
      ctyps
      (AddSetCookieApiVerb a)

type instance AddSetCookieApi Raw = Raw

type instance
  AddSetCookieApi (Stream method stat framing ctyps a) =
    Stream method stat framing ctyps (AddSetCookieApiVerb a)

type instance AddSetCookieApi (Headers hs a) = AddSetCookieApiVerb (Headers hs a)

data SetCookieList (n :: Nat) :: Type where
  SetCookieNil :: SetCookieList 'Z
  SetCookieCons :: Maybe SetCookie -> SetCookieList n -> SetCookieList ('S n)

class AddSetCookies (n :: Nat) orig new where
  addSetCookies :: SetCookieList n -> orig -> new

instance
  {-# OVERLAPS #-}
  AddSetCookies ('S n) oldb newb =>
  AddSetCookies ('S n) (a -> oldb) (a -> newb)
  where
  addSetCookies cookies oldfn = addSetCookies cookies . oldfn

instance AddSetCookies 'Z orig orig where
  addSetCookies _ = identity

instance
  {-# OVERLAPPABLE #-}
  ( Functor m
  , AddSetCookies n (m old) (m cookied)
  , AddHeader "Set-Cookie" SetCookie cookied new
  ) =>
  AddSetCookies ('S n) (m old) (m new)
  where
  addSetCookies (mCookie `SetCookieCons` rest) oldVal = case mCookie of
    Nothing -> noHeader <$> addSetCookies rest oldVal
    Just cookie -> addHeader cookie <$> addSetCookies rest oldVal

instance
  {-# OVERLAPS #-}
  (AddSetCookies ('S n) a a', AddSetCookies ('S n) b b') =>
  AddSetCookies ('S n) (a :<|> b) (a' :<|> b')
  where
  addSetCookies cookies (a :<|> b) = addSetCookies cookies a :<|> addSetCookies cookies b

instance
  {-# OVERLAPS #-}
  ( AddSetCookies ('S n) (ServerT (ToServantApi api) m) cookiedApi
  , Generic (api (AsServerT m))
  , GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  ) =>
  AddSetCookies ('S n) (api (AsServerT m)) cookiedApi
  where
  addSetCookies cookies = addSetCookies cookies . toServant

-- | servant >=0.11
instance AddSetCookies ('S n) (Tagged m Application) (Tagged m Application) where
  addSetCookies cookies r = Tagged $ \request reply ->
    unTagged r request $ reply . mapResponseHeaders (<> mkHeaders cookies)

mkHeaders :: SetCookieList x -> [HTTP.Header]
mkHeaders x = ("Set-Cookie",) <$> mkCookies x
  where
    mkCookies :: forall y. SetCookieList y -> [BS.ByteString]
    mkCookies SetCookieNil = []
    mkCookies (SetCookieCons Nothing rest) = mkCookies rest
    mkCookies (SetCookieCons (Just y) rest) =
      (BSL.toStrict . toLazyByteString) (renderSetCookie y) : mkCookies rest
