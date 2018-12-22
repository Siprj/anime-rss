{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.DataModel
    ( DataModel(..)
    , defaultDataModel
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Maybe (Maybe(Nothing))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector, fromList)
import GHC.Generics (Generic)
import Text.Show (Show)

import DataModel.Type.Feed (Feed)


data DataModel = DataModel
    { feeds :: Vector Feed
    , lastFeedsModification :: Maybe UTCTime
    }
  deriving (Eq, Typeable, Generic, Data, Show)

defaultDataModel :: DataModel
defaultDataModel = DataModel (fromList []) Nothing

$(deriveSafeCopy 0 'base ''DataModel)
