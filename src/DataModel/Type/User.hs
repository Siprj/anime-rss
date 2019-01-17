{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataModel.Type.User
    ( User'(..)
    , User
    , AddUser
    , email
    , id
    , name
    , password
    )
  where

import Control.Applicative ((<*>))
import Control.Lens.TH (makeLenses)
import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.SafeCopy
    ( SafeCopy(putCopy, getCopy, kind, version)
    , contain
    , base
    , safeGet
    , safePut
    )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

import DataModel.Type.Apply (Apply, ApplyType(Drop, Keep))
import DataModel.Type.Id (UserId)


data User' a = User'
    { _id :: Apply a UserId
    , _name :: Text
    , _email :: Text
    , _password :: ByteString
    }

makeLenses ''User'

deriving instance Generic (User' a)

type User = User' 'Keep
type AddUser = User' 'Drop

deriving instance Show User
deriving instance Eq User

instance (SafeCopy (Apply a UserId)) => SafeCopy (User' a) where
    version = 1
    kind = base
    putCopy User'{..} = contain $ do
        safePut _id
        safePut _name
        safePut _email
        safePut _password

    getCopy = contain $ User'
        <$> safeGet
        <*> safeGet
        <*> safeGet
        <*> safeGet

deriving instance Show AddUser
deriving instance Eq AddUser
