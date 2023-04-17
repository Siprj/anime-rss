{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ResourceAttributes (
  resourceAttributes,
) where

import Control.Applicative (pure)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.Text (pack)
import Data.Text.IO (readFile)
import Otel.Type (
  KeyValue (KeyValue),
  ResourceAttributes,
  Value (StringV),
 )
import System.Environment (lookupEnv)
import System.IO (IO)

resourceAttributes :: IO ResourceAttributes
resourceAttributes = do
  containerName <- maybe "" pack <$> lookupEnv "OTEL_CONTAINER_NAME"
  containerId <- readFile "/etc/hostname"
  pure
    [ KeyValue "container.id" $ StringV containerId
    , KeyValue "container.name" $ StringV containerName
    ]
