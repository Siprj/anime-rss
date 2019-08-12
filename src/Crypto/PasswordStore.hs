{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.PasswordStore
--    (..
--    )
  where

import GHC.Generics(Generic)
import Control.Applicative (pure)
import qualified Crypto.KDF.Argon2 as Crypto
    ( Options(Options, version, variant, memory, parallelism, iterations)
    , Variant(Argon2id)
    , Version(Version13)
    , hash
    )
import Crypto.Error (CryptoFailable)
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL)
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq ((==))
import Data.Eq (Eq)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Serialize (Serialize(put, get))
import qualified Data.SafeCopy as SC
    ( SafeCopy(putCopy, getCopy, kind, version)
    , base
    )
import System.IO (IO)
import Text.Show (Show)


data HashParameters = HashParameters
    { hashParametersOptions :: Crypto.Options
    , hashParametersOutputHashSize :: Int
    , hashParametersSalt :: ByteString
    }
  deriving (Eq, Show, Generic)

instance Serialize HashParameters where
    put = put
    get = get

type PasswordHash = (HashParameters, ByteString)

newtype SerializableOptions = SerializableOptions Crypto.Options
instance SC.SafeCopy SerializableOptions where
    version = 0
    kind = SC.base
    putCopy = SC.putCopy
    getCopy = SC.getCopy


instance SC.SafeCopy HashParameters where
    version = 0
    kind = SC.base
    putCopy = SC.putCopy
    getCopy = SC.getCopy

-- | Default Options:
-- TimeCost: Default is 3.
-- MemoryCost: Memory usage of 2^N KiB.
-- Looks like good default is 12. But
-- https://www.reddit.com/r/crypto/comments/85jdsf/choosing_argon2_parameters_for_keepass/
-- suggests much grater number.
-- Parallelism: Default 1. But I would suggest to use grater number, probably
-- as the number of CPU cores.
defaultOptions :: Crypto.Options
defaultOptions = Crypto.Options
    { Crypto.iterations = 3
    , Crypto.memory = 1 `shiftL` 12
    , Crypto.parallelism = 1
    , Crypto.variant = Crypto.Argon2id
    , Crypto.version = Crypto.Version13
    }

hashPassword
    :: ByteString
    -- ^ password
    -> Crypto.Options
    -> Int
    -- ^ Output hash size. Default is 32.
    -> IO (CryptoFailable PasswordHash)
hashPassword password options hashSize = do
    salt <- getRandomBytes 16
    pure $ (HashParameters options hashSize salt, )
        <$> Crypto.hash options password salt hashSize

verifyPassword
    :: ByteString
    -- ^ password
    -> PasswordHash
    -> CryptoFailable Bool
verifyPassword password (HashParameters options hashSize salt, passwordHash) = do
    (passwordHash ==) <$> Crypto.hash options password salt hashSize
