{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.Old.Feed
    ( Feed_v1(..)
    , SetFeed_v1(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.SafeCopy
    ( Migrate(MigrateFrom, migrate)
    , base
    , deriveSafeCopy
    , extension
    )
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.URI (URI)
import Text.Show (Show)

import DataModel.OrphanInstances ()


data Feed_v0 = Feed_v0
    { name_v0 :: Text
    , url_v0 :: URI
    , date_v0 :: UTCTime
    , imgUrl_v0 :: URI
    }
  deriving (Eq, Typeable, Generic, Data, Show)

$(deriveSafeCopy 0 'base ''Feed_v0)

data SetFeed_v0 = SetFeed_v0
    { setFeedName_v0 :: Text
    , setFeedUrl_v0 :: URI
    , setFeedImgUrl_v0 :: URI
    }

$(deriveSafeCopy 0 'base ''SetFeed_v0)

data Feed_v1 = Feed_v1
    { name_v1 :: Text
    , url_v1 :: URI
    , date_v1 :: UTCTime
    , imgUrl_v1 :: URI
    , episodeNumber_v1 :: Int
    }
  deriving (Eq, Typeable, Generic, Data, Show)

$(deriveSafeCopy 1 'extension ''Feed_v1)

instance Migrate Feed_v1 where
    type MigrateFrom Feed_v1 = Feed_v0
    migrate Feed_v0{..} = Feed_v1
        { name_v1 = name_v0
        , url_v1 = url_v0
        , imgUrl_v1 = imgUrl_v0
        , date_v1 = date_v0
        , episodeNumber_v1 = 0
        }

data SetFeed_v1 = SetFeed_v1
    { setFeedName_v1 :: Text
    , setFeedUrl_v1 :: URI
    , setFeedImgUrl_v1 :: URI
    , setFeedEpisodeNumber_v1 :: Int
    }

$(deriveSafeCopy 1 'extension ''SetFeed_v1)

instance Migrate SetFeed_v1 where
    type MigrateFrom SetFeed_v1 = SetFeed_v0
    migrate SetFeed_v0{..} = SetFeed_v1
        { setFeedName_v1 = setFeedName_v0
        , setFeedUrl_v1 = setFeedUrl_v0
        , setFeedImgUrl_v1 = setFeedImgUrl_v0
        , setFeedEpisodeNumber_v1 = 0
        }
