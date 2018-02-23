{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.Feed
    ( Feed(..)
    , SetFeed(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.SafeCopy
    ( Migrate(MigrateFrom, migrate)
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
import DataModel.Type.Old.Feed
    ( Feed_v0(Feed_v0, name_v0, url_v0, date_v0, imgUrl_v0)
    , SetFeed_v0(SetFeed_v0, setFeedName_v0, setFeedUrl_v0, setFeedImgUrl_v0)
    )


data Feed = Feed
    { name :: Text
    , url :: URI
    , date :: UTCTime
    , imgUrl :: URI
    , episodeNumber :: Int
    }
  deriving (Eq, Typeable, Generic, Data, Show)

$(deriveSafeCopy 1 'extension ''Feed)

instance Migrate Feed where
    type MigrateFrom Feed = Feed_v0
    migrate Feed_v0{..} = Feed
        { name = name_v0
        , url = url_v0
        , imgUrl = imgUrl_v0
        , date = date_v0
        , episodeNumber = 0
        }

data SetFeed = SetFeed
    { setFeedName :: Text
    , setFeedUrl :: URI
    , setFeedImgUrl :: URI
    , setFeedEpisodeNumber :: Int
    }

instance Migrate SetFeed where
    type MigrateFrom SetFeed = SetFeed_v0
    migrate SetFeed_v0{..} = SetFeed
        { setFeedName = setFeedName_v0
        , setFeedUrl = setFeedUrl_v0
        , setFeedImgUrl = setFeedImgUrl_v0
        , setFeedEpisodeNumber = 0
        }

$(deriveSafeCopy 1 'extension ''SetFeed)
