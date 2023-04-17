{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ResourceAttributes
  (
  resourceAttributes)
where

import Otel.Type
    ( KeyValue(KeyValue), ResourceAttributes, Value(StringV) )
import Data.Function (($))
import System.IO ( IO )
import Control.Applicative (pure)
import Data.Text.IO (readFile)
import System.Environment (lookupEnv)
import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Maybe (maybe)

resourceAttributes :: IO ResourceAttributes
resourceAttributes = do
  containerName <- maybe "" pack <$> lookupEnv "OTEL_CONTAINER_NAME"
  containerId <- readFile "/etc/hostname"
  pure
    [ KeyValue "container.id" $ StringV containerId
    , KeyValue "container.name" $ StringV containerName
    ]
