{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
module Configuration
  ( getConfiguration
  , Configuration(..)
  )
where

import System.Environment (lookupEnv)
import Text.Show (Show)
import Data.String (String)
import System.IO (IO)
import Data.Maybe (maybe)
import Control.Applicative (pure, (<$>))
import Control.Monad ( Monad((>>=)), MonadFail(fail) )
import Data.Function ( ($))
import Data.Semigroup ( Semigroup((<>)) )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import GHC.Generics (Generic)


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
  pure Configuration{..}
