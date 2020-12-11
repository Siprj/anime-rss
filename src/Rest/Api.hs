{-# LANGUAGE DeriveAnyClass #-}
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
    , Protected
    , Anime(..)
    , Login(..)
    , PostAnimeFollow(..)
    , User(..)
    , ChannelId
    )
  where

import Core.Type.Id (UserId, AnimeId)
import Core.Type.User (Email)
import Web.Cookie (SetCookie)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.URI (URI)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Rest.AtomMime (AtomFeed)
import Rest.Authentication (UserAuthentication)
import Servant.API (Header, Headers, Capture, ReqBody, (:<|>), (:>), Get, JSON, Post)
import Text.Feed.Types (Feed)
import Text.Show (Show)
import Servant (NoContent)


type ChannelId = UUID

data User = User
    { userId ::UserId
    , email :: Email
    , name :: Text
    , episodeChannel :: UUID
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''User

data Login = Login
    { email :: Email
    , password :: Text
    }

makeFieldLabelsWith noPrefixFieldLabels ''Login

data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    , following :: Bool
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
    :<|> "followe/anime" :> ReqBody '[JSON] [PostAnimeFollow] :> Post '[JSON] [Anime]
    :<|> "animes" :> Get '[JSON] [Anime]

type Api
    = "atom" :> Get '[AtomFeed] Feed
    :<|> "atom" :> "episodes" :> Capture "channel-id" ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "animes" :> Capture "channel-id" ChannelId :> Get '[AtomFeed] Feed
    :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)
    :<|> UserAuthentication :> Protected
