{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rest.Api
    ( Api
    , Login(..)
    , User(..)
    )
  where

import Core.Type.Id (UserId, AnimeId)
import Core.Type.User (Email)
import Data.Eq (Eq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.URI (URI)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Rest.AtomMime (AtomFeed)
import Servant.API (ReqBody, PostNoContent, (:<|>), (:>), Get, JSON)
import Servant.Auth.Server (Auth, JWT)
import Text.Feed.Types (Feed)
import Text.Show (Show)
import Data.Bool (Bool)


type ChannelId = UUID

data User = User
    { userId ::UserId
    , email :: Email
    , name :: Text
    , animeChannel :: UUID
    , episodeChannel :: UUID
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''User

data Login = Login
    { email :: Email
    , password :: Text
    }

makeFieldLabelsWith noPrefixFieldLabels ''Login

data UserSession = UserSession
    { userId :: UserId
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''UserSession

data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Anime

data PostAnimeFollow = PostAnimeFollow
    { animeId :: AnimeId
    , follow :: Bool
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''PostAnimeFollow

type Protected
    = "user" :> Get '[JSON] User
    :<|> "anime" :> ReqBody '[JSON] [PostAnimeFollow] :> PostNoContent

type Api
    = "atom" :> Get '[AtomFeed] Feed
    :<|> "atom" :> "episodes" :> ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "anime" :> ChannelId :> Get '[AtomFeed] Feed
    :<|> "login" :> ReqBody '[JSON] Login :> PostNoContent
    :<|> "anime" :> Get '[JSON] [Anime]
    :<|> Servant.Auth.Server.Auth '[JWT] UserSession :> Protected
