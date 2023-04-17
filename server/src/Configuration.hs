{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Configuration (
  getConfiguration,
  Configuration (..),
) where

import Control.Applicative (pure, (<$>))
import Control.Monad (Monad ((>>=)), MonadFail (fail))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (String)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.IO (IO)
import Text.Show (Show)

data Configuration = Configuration
  { databaseConnectionString :: BS.ByteString
  , logEnpoint :: String
  , traceEndpoint :: String
  }
  deriving stock (Generic, Show)

getEnv' :: String -> IO String
getEnv' name = lookupEnv name >>= maybe error' pure
  where
    error' :: IO String
    error' = fail $ "Error: " <> name <> " environment variable not found!"

getConfiguration :: IO Configuration
getConfiguration = do
  databaseConnectionString <- BSC.pack <$> getEnv' "DATABASE_CONNECTION_STRING"
  logEnpoint <- getEnv' "OTEL_LOG_ENDPOINT"
  traceEndpoint <- getEnv' "OTEL_TRACE_ENDPOINT"
  pure Configuration {..}
