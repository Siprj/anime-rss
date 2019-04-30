{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.PasswordStore
--    (..
--    )
  where

import Control.Applicative (pure)
import Crypto.KDF.Argon2
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
import System.IO (IO)


data HashParameters = HashParameters
    { hashParametersOptions :: Options
    , hashParametersOutputHashSize :: Int
    , hashParametersSalt :: ByteString
    }
  deriving (Eq)

type PasswordHash = (HashParameters, ByteString)

-- | Default Options:
-- TimeCost: Default is 3.
-- MemoryCost: Memory usage of 2^N KiB.
-- Looks like good default is 12. But
-- https://www.reddit.com/r/crypto/comments/85jdsf/choosing_argon2_parameters_for_keepass/
-- suggests much grater number.
-- Parallelism: Default 1. But I would suggest to use grater number, probably
-- as the number of CPU cores.
defaultOptions :: Options
defaultOptions = Options
    { iterations = 3
    , memory = 1 `shiftL` 12
    , parallelism = 1
    , variant = Argon2id
    , version = Version13
    }

hashPassword
    :: ByteString
    -- ^ password
    -> Options
    -> Int
    -- ^ Output hash size. Default is 32.
    -> IO (CryptoFailable PasswordHash)
hashPassword password options hashSize = do
    salt <- getRandomBytes 16
    pure $ (HashParameters options hashSize salt, ) <$> hash options password salt hashSize

verifyPassword
    :: ByteString
    -- ^ password
    -> PasswordHash
    -> CryptoFailable Bool
verifyPassword password (HashParameters options hashSize salt, passwordHash) = do
    (passwordHash ==) <$> hash options password salt hashSize
