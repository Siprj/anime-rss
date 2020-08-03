{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Rest.Types
    ( User(..)
    , ChannelId
    )
  where

import Data.Text (Text)
import Data.UUID (UUID)

type ChannelId = UUID

data User = User
    { name :: Text
    , email :: Text
    }
