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
    , LoggedInUser(..)
    )
  where

import Core.Type.Id (UserId, AnimeId)
import Core.Type.User (Email)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Rest.AtomMime (AtomFeed)
import Servant.API (Header, Headers, Capture, ReqBody, (:<|>), (:>), Get, JSON, Post)
import Text.Feed.Types (Feed)
import Text.Show (Show)
import Servant (NoContent, Verb, StdMethod (POST))
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Servant.Auth.Server (SetCookie, Auth, Cookie, ToJWT, FromJWT)


newtype LoggedInUser = LoggedInUser
    { userId :: UserId
    }
  deriving (Show, Eq, Generic)

instance ToJSON LoggedInUser
instance ToJWT LoggedInUser
instance FromJSON LoggedInUser
instance FromJWT LoggedInUser


type ChannelId = UUID

data User = User
    { userId ::UserId
    , email :: Email
    , name :: Text
    , episodeChannel :: UUID
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''User
instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON User

data Login = Login
    { email :: Email
    , password :: Text
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Login
instance ToJSON Login where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Login

data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: Text
    , imageUrl :: Text
    , date :: UTCTime
    , following :: Bool
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Anime
instance ToJSON Anime where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Anime

data PostAnimeFollow = PostAnimeFollow
    { animeId :: AnimeId
    , follow :: Bool
    }
  deriving (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''PostAnimeFollow
instance ToJSON PostAnimeFollow where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON PostAnimeFollow

type Protected
    = "user" :> Get '[JSON] User
    :<|> "followe" :> "anime" :> ReqBody '[JSON] [PostAnimeFollow] :> Verb 'POST 204 '[JSON] NoContent
    :<|> "animes" :> Get '[JSON] [Anime]

type Api = "atom" :> "episodes" :> Capture "channel-id" ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "animes" :> Get '[AtomFeed] Feed
    :<|> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> Servant.Auth.Server.Auth '[Cookie] LoggedInUser :> Protected
