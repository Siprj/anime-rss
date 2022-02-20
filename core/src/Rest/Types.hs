{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
