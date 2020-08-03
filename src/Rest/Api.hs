{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Rest.Api
    (Api)
  where

import Data.UUID (UUID)
import Servant.API ((:<|>), (:>), Get)
import Text.Feed.Types (Feed)

import Rest.AtomMime (AtomFeed)


type ChannelId = UUID

type Api
    = "atom" :> Get '[AtomFeed] Feed
    :<|> "atom" :> ChannelId :> Get '[AtomFeed] Feed
    -- TODO: Auth. this endpoint
    :<|> "user" :> Get '[AtomFeed] Feed
