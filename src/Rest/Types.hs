{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Rest.Types
    ( UserBrief(..)
    , ChannelId
    )
  where

import Data.Text (Text)
import Data.UUID (UUID)

type ChannelId = UUID

data UserBrief = UserBrief
    { name :: Text
    , email :: Text
    }
