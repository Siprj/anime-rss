{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.Old.Feed
    ( Feed_v0(..)
    , SetFeed_v0(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.SafeCopy
    ( base
    , deriveSafeCopy
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
