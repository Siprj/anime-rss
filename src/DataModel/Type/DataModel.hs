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
    , createDefaultDataModel
    , feeds
    , users
    , usersIdCounter
    , lastFeedsModification
    )
  where

import Control.Lens.TH (makeLenses)
import Control.Applicative (pure)
import Control.Monad ((>>=))
import Crypto.Error (throwCryptoErrorIO)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe(Nothing))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector, fromList)
import Data.Map.Strict (Map, singleton)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import Crypto.PasswordStore (hashPassword, defaultOptions)
import DataModel.Type.Id (toId, UserId)
import DataModel.Type.Feed (Feed)
import qualified DataModel.Type.User as User
    (User, User'(User', _id, _name, _email, _password))


data DataModel = DataModel
    { _feeds :: Vector Feed
    , _users :: Map UserId User.User
    , _userLastId :: UserId
    , _usersIdCounter :: Int
    , _lastFeedsModification :: Maybe UTCTime
    }
  deriving (Eq, Typeable, Generic, Show)

makeLenses ''DataModel

createDefaultDataModel :: IO DataModel
createDefaultDataModel = do
    password <- hashPassword "password" defaultOptions 32
        >>= throwCryptoErrorIO
    pure $ DataModel
        { _feeds = fromList []
        , _users = singleton 0 $ User.User'
            { _id = toId 0
            , _name = "Admin"
            , _email = "admin@admin.com"
            , _password = password
            }
        , _usersIdCounter = 1
        , _lastFeedsModification = Nothing
        }

$(deriveSafeCopy 0 'base ''DataModel)
