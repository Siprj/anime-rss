{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataModel.Tables
    ( GetEpisode(..)
    , CreateEpisode(..)
    )
  where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.URI (URI)
import Data.Text (Text)
import Data.Time (UTCTime)
import DataModel.Persistent (AnimeId)
import GHC.Show (Show)
import Data.ByteString (ByteString)


data GetEpisode = GetEpisode
    { id :: UUID
    , url :: URI
    , number :: Text
    , date :: UTCTime
    , animeId :: AnimeId
    }
  deriving stock (Show, Generic)

data CreateEpisode = CreateEpisode
    { url :: URI
    , number :: Text
    , date :: UTCTime
    , animeId :: AnimeId
    }
  deriving stock (Show, Generic)

data CreateUser = CreateUser
    { name :: Text
    , email :: Text
    , password :: ByteString
    , newsChannel :: UUID
    }
  deriving stock (Show, Generic)

data GetUser = GetUser
    { id :: UUID
    , name :: Text
    , email :: Text
    , password :: ByteString
    , newsChannel :: UUID
    }
  deriving stock (Show, Generic)

data TemporaryKey = TemporaryKey
   { key :: UUID
   , until :: UTCTime
   , userId :: UUID
   , userEmail :: Text
   }
  deriving stock (Show, Generic)

data CreateAnime = CreateAnime
    { title :: Text
    , imgUrl :: Text
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)

data GetAnime = GetAnime
    { id :: UUID
    , title :: Text
    , imgUrl :: Text
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)

data UserFollows = UserFollows
    { userId :: UUID
    , animeId :: UUID
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)
