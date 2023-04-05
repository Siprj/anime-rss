{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AnimeRss.Rest.Api (
  Api,
  Protected,
  Anime (..),
  Login (..),
  PostAnimeFollow (..),
  User (..),
  ChannelId,
  LoggedInUser (..),
  SubParam (..),
) where

import AnimeRss.Ids (AnimeId, UserId)
import AnimeRss.Rest.AtomMime (AtomFeed)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericParseJSON, genericToEncoding, parseJSON, toEncoding)
import Data.Bool (Bool)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Servant (NoContent, StdMethod (POST), Verb)
import Servant.API (Capture, FromHttpApiData (..), Get, Header, Headers, JSON, Post, QueryParam, ReqBody, ToHttpApiData (..), (:<|>), (:>))
import AnimeRss.Rest.Auth
import Text.Feed.Types (Feed)
import Text.Read (Read, readMaybe)
import Text.Show (Show, show)
import Web.Cookie

type Email = Text

newtype LoggedInUser = LoggedInUser
  { userId :: UserId
  }
  deriving stock (Show, Eq, Generic)

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

type Protected =
  "user" :> Get '[JSON] User
    :<|> "follow" :> "anime" :> ReqBody '[JSON] PostAnimeFollow :> Verb 'POST 204 '[JSON] NoContent
    :<|> "animes" :> QueryParam "sub" SubParam :> QueryParam "search" Text :> Get '[JSON] [Anime]

type Api =
  "atom" :> "episodes" :> Capture "channel-id" ChannelId :> Get '[AtomFeed] Feed
    :<|> "atom" :> "animes" :> Get '[AtomFeed] Feed
    :<|> "api" :> "login" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "api" :> APIAuthProtect "session" :> Protected
