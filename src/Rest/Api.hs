{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Rest.Api
    ( Api
    , Login(..)
    , User(..)
    )
  where

import GHC.Generics (Generic)
import Data.UUID (UUID)
import Servant.API (ReqBody, PostNoContent, (:<|>), (:>), Get, JSON)
import Servant.Auth.Server (Auth, JWT)
import Text.Feed.Types (Feed)
import Core.Type.Id (UserId)
import Core.Type.User (Email)

import Rest.AtomMime (AtomFeed)
import Text.Show (Show)
import Data.Eq (Eq)
import Data.Text (Text)


type ChannelId = UUID

data User = User
    { userId ::UserId
    , email :: Email
    , name :: Text
    , animeChannel :: UUID
    , episodeChannel :: UUID
    }
  deriving (Show, Eq, Generic)

data Login = Login
    { email :: Email
    , password :: Text
    }

data UserSession = UserSession
    { userId :: UserId
    }
  deriving (Show, Eq, Generic)

type Api
    = "atom" :> Get '[AtomFeed] Feed
    :<|> "atom" :> "episodes" :> ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "anime" :> ChannelId :> Get '[AtomFeed] Feed
    :<|> "login" :> ReqBody '[JSON] Login :> PostNoContent
    :<|> (Servant.Auth.Server.Auth '[JWT] UserSession :<|> "user" :> Get '[JSON] User)
