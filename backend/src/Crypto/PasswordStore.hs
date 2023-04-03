{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crypto.PasswordStore (
  HashParameters,
  PasswordHash,
  defaultOptions,
  hashPassword,
  verifyPassword,
) where

import Control.Applicative (pure)
import Crypto.Error (CryptoFailable)
import Crypto.KDF.Argon2 qualified as Crypto (
  Options (Options, iterations, memory, parallelism, variant, version),
  Variant (Argon2d, Argon2i, Argon2id),
  Version (Version10, Version13),
  hash,
 )
import Crypto.Random (getRandomBytes)
import Data.Bits (shiftL)
import Data.Bool (Bool)
import Data.ByteString (ByteString)
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Semigroup (Semigroup ((<>)))
import Data.Serialize (Get, Put, Serialize (get, put), getWord8, putWord8)
import Data.Serialize.Put ()
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show (show))
import Prelude (error)

data HashParameters = HashParameters
  { hashParametersOptions :: Crypto.Options
  , hashParametersOutputHashSize :: Int
  , hashParametersSalt :: ByteString
  }
  deriving stock (Eq, Show, Generic)

putVariant :: Crypto.Variant -> Put
putVariant v = putWord8 $ case v of
  Crypto.Argon2d -> 0
  Crypto.Argon2i -> 1
  Crypto.Argon2id -> 2

getVariant :: Get Crypto.Variant
getVariant = mkVariant <$> getWord8
  where
    mkVariant = \case
      0 -> Crypto.Argon2d
      1 -> Crypto.Argon2i
      2 -> Crypto.Argon2id
      v -> error $ "Can't parse Argon2 variant. Expected <0-2> got: " <> show v

putVersion :: Crypto.Version -> Put
putVersion v = putWord8 $ case v of
  Crypto.Version10 -> 0
  Crypto.Version13 -> 1

getVersion :: Get Crypto.Version
getVersion = mkVariant <$> getWord8
  where
    mkVariant = \case
      0 -> Crypto.Version10
      1 -> Crypto.Version13
      v -> error $ "Can't parse Argon2 version. Expected <0-1> got: " <> show v

instance Serialize HashParameters where
  put (HashParameters Crypto.Options {..} hashSize salt) = do
    put iterations
    put memory
    put parallelism
    putVariant variant
    putVersion version
    put hashSize
    put salt
  get = do
    iterations <- get
    memory <- get
    parallelism <- get
    variant <- getVariant
    version <- getVersion
    hashSize <- get
    HashParameters Crypto.Options {..} hashSize <$> get

type PasswordHash = (HashParameters, ByteString)

-- | Default Options:
-- TimeCost: Default is 3.
-- MemoryCost: Memory usage of 2^N KiB.
-- Looks like good default is 12. But
-- https://www.reddit.com/r/crypto/comments/85jdsf/choosing_argon2_parameters_for_keepass/
-- suggests much grater number.
-- Parallelism: Default 1. But I would suggest to use grater number, probably
-- as the number of CPU cores.
defaultOptions :: Crypto.Options
defaultOptions =
  Crypto.Options
    { Crypto.iterations = 3
    , Crypto.memory = 1 `shiftL` 12
    , Crypto.parallelism = 1
    , Crypto.variant = Crypto.Argon2id
    , Crypto.version = Crypto.Version13
    }

hashPassword ::
  -- | password
  ByteString ->
  Crypto.Options ->
  -- | Output hash size. Default is 32.
  Int ->
  IO (CryptoFailable PasswordHash)
hashPassword password options hashSize = do
  salt <- getRandomBytes 16
  pure $
    (HashParameters options hashSize salt,)
      <$> Crypto.hash options password salt hashSize

verifyPassword ::
  -- | password
  ByteString ->
  PasswordHash ->
  CryptoFailable Bool
verifyPassword password (HashParameters options hashSize salt, passwordHash) =
  (passwordHash ==) <$> Crypto.hash options password salt hashSize
