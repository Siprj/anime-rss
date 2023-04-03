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
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module AnimeRss.Rest.Api
    ( Api
    , Protected
    , Anime(..)
    , Login(..)
    , PostAnimeFollow(..)
    , User(..)
    , ChannelId
    , LoggedInUser(..)
    , SubParam(..)
    )
  where

import AnimeRss.Ids (UserId, AnimeId)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Text (Text, unpack, pack)
import Data.Semigroup ((<>))
import Data.Maybe (maybe)
import Data.Function ((.), ($))
import Data.Either (Either(Right, Left))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import AnimeRss.Rest.AtomMime (AtomFeed)
import Servant.API (Header, Headers, Capture, ReqBody, (:<|>), (:>), Get, JSON, Post, QueryParam, FromHttpApiData(..), ToHttpApiData(..))
import Text.Feed.Types (Feed)
import Text.Show (Show, show)
import Text.Read (Read, readMaybe)
import Servant (NoContent, Verb, StdMethod (POST))
import Data.Aeson (ToJSON, FromJSON, parseJSON, toEncoding, genericToEncoding, defaultOptions, genericParseJSON)
import Servant.Auth.Server (SetCookie, Auth, Cookie, ToJWT, FromJWT)

type Email = Text
newtype LoggedInUser = LoggedInUser
    { userId :: UserId
    }
  deriving stock (Show, Eq, Generic)

instance ToJSON LoggedInUser
instance ToJWT LoggedInUser
instance FromJSON LoggedInUser
instance FromJWT LoggedInUser


type ChannelId = UUID

data User = User
    { userId :: UserId
    , email :: Email
    , name :: Text
    , episodeChannel :: UUID
    }
  deriving stock (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''User
instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON User

data Login = Login
    { email :: Email
    , password :: Text
    }
  deriving stock (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Login
instance ToJSON Login where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Login where
    parseJSON = genericParseJSON defaultOptions

data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: Text
    , imageUrl :: Text
    , date :: UTCTime
    , following :: Bool
    }
  deriving stock (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''Anime
instance ToJSON Anime where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Anime where
    parseJSON = genericParseJSON defaultOptions

data PostAnimeFollow = PostAnimeFollow
    { animeId :: AnimeId
    , follow :: Bool
    }
  deriving stock (Show, Eq, Generic)

makeFieldLabelsWith noPrefixFieldLabels ''PostAnimeFollow
instance ToJSON PostAnimeFollow where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON PostAnimeFollow where
    parseJSON = genericParseJSON defaultOptions

data SubParam
    = All
    | Subscribed
    | Unsubscribed
  deriving stock (Show, Read, Eq, Generic)

instance ToHttpApiData SubParam where
  toUrlPiece = pack . show
  toQueryParam = pack . show

instance FromHttpApiData SubParam where
  parseUrlPiece t = maybe (Left $ "Expected values \"All\", \"Subscribed\", \"Unsubscribed\"; received: " <> t) Right . readMaybe $ unpack t
  parseQueryParam t = maybe (Left $ "Expected values \"All\", \"Subscribed\", \"Unsubscribed\"; received: " <> t) Right . readMaybe $ unpack t

type Protected
    = "user" :> Get '[JSON] User
    :<|> "follow" :> "anime" :> ReqBody '[JSON] PostAnimeFollow :> Verb 'POST 204 '[JSON] NoContent
    :<|> "animes" :> QueryParam "sub" SubParam :>  QueryParam "search" Text :> Get '[JSON] [Anime]

type Api = "atom" :> "episodes" :> Capture "channel-id" ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "animes" :> Get '[AtomFeed] Feed
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "api" :> Servant.Auth.Server.Auth '[Cookie] LoggedInUser :> Protected
