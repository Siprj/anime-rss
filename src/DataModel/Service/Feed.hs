{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DataModel.Service.Feed
    ( addFeedIfUnique
    , listFeeds
    )
  where

import Data.Acid (AcidState, query, update)
import Data.Maybe (Maybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector (Vector)
import System.IO (IO)

import DataModel.Type.DataModel (DataModel)
import DataModel.Type.Feed (Feed, SetFeed)
import DataModel.Acid(AddFeedIfUnique(AddFeedIfUnique), ListFeeds(ListFeeds))


addFeedIfUnique :: AcidState DataModel -> SetFeed -> IO ()
addFeedIfUnique state feed = do
    now <- getCurrentTime
    update state (AddFeedIfUnique feed now)

listFeeds :: AcidState DataModel -> IO (Vector Feed, Maybe UTCTime)
listFeeds state = query state ListFeeds

