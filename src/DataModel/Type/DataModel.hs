{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , feeds
    , users
    , usersIdCounter
    , lastFeedsModification
    )
  where

import Control.Lens.TH (makeLenses)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector, fromList)
import Data.IntMap (IntMap, singleton)
import GHC.Generics (Generic)
import Text.Show (Show)

import DataModel.Type.Id (Id(Id))
import DataModel.Type.Feed (Feed)
import qualified DataModel.Type.User as User
    (User, User'(User', _id, _name, _email, _password))


data DataModel = DataModel
    { _feeds :: Vector Feed
    , _users :: IntMap User.User
    , _usersIdCounter :: Int
    , _lastFeedsModification :: Maybe UTCTime
    }
  deriving (Eq, Typeable, Generic, Show)

makeLenses ''DataModel

defaultDataModel :: DataModel
defaultDataModel = DataModel
    { _feeds = fromList []
    , _users = singleton 0 $ User.User'
        { _id = Id 0
        , _name = "Admin"
        , _email = "admin@admin.com"
        , _password = "asdf"
        }
    , _usersIdCounter = 1
    , _lastFeedsModification = Nothing
    }

$(deriveSafeCopy 0 'base ''DataModel)
