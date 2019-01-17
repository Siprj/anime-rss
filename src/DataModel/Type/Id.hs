{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
{-# LANGUAGE FlexibleInstances #-}

module DataModel.Type.Id
    ( Id(Id)
    , UserId
    , fromId
    , toId
    )
  where

import Data.Data (Data)
import Data.Function (($))
import Data.Int (Int)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude (Num((+), (-), (*), negate, signum, abs, fromInteger), Eq)
import Data.Functor ((<$>))
import Data.SafeCopy
    ( SafeCopy(putCopy, getCopy, kind, version)
    , contain
    , base
    , safeGet
    , safePut
    )
import Text.Show (Show)


newtype Id a = Id Int
  deriving (Generic, Eq, Typeable, Data, Show)

instance Num (Id a) where
    (Id a) + (Id b) = Id $ a + b
    (Id a) - (Id b) = Id $ a - b
    (Id a) * (Id b) = Id $ a * b
    abs (Id a) = Id $ abs a
    negate (Id a) = Id $ negate a
    signum (Id a) = Id $ signum a
    fromInteger a = Id $ fromInteger a

data User'

type UserId = Id User'

instance SafeCopy (Id a) where
    version = 1
    kind = base
    putCopy (Id id) = putCopy id
    getCopy = contain $ Id <$> safeGet

fromId :: Id a -> Int
fromId (Id a) = a

toId :: Int -> Id a
toId = Id
