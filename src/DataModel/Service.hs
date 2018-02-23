{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DataModel.Service
    ( DataModel
    , addFeedIfUnique
    , createDataModelChannel
    , listFeeds
    , runDataModel
    , runDataModelEffect
    )
  where

import Control.Monad ((>>=), (>>))
import Control.Monad.Freer (Eff, Member, send)
import Data.Acid (AcidState)
import Data.Function (($), (.))
import Data.Maybe (Maybe)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import System.IO (IO)

import Control.Monad.Freer.Service
    ( IscCall(ChannelData, get, put)
    , ServiceChannel
    , createServiceChannel
    , runServiceChannel
    , runServiceEffect
    )
import qualified DataModel.Type.DataModel as DataModel (DataModel)
import DataModel.Type.Feed (Feed, SetFeed)
import qualified DataModel.Service.Feed as Service (addFeedIfUnique, listFeeds)


data DataModel s where
    AddFeedIfUnique :: SetFeed -> DataModel ()
    ListFeeds :: DataModel (Vector Feed, Maybe UTCTime)

addFeedIfUnique :: Member DataModel effs => SetFeed -> Eff effs ()
addFeedIfUnique = send . AddFeedIfUnique

listFeeds :: Member DataModel effs => Eff effs (Vector Feed, Maybe UTCTime)
listFeeds = send ListFeeds

instance IscCall DataModel where
    data ChannelData DataModel a = WrapDataModel (DataModel a)

    get :: ChannelData DataModel a -> DataModel a
    get (WrapDataModel v) = v

    put :: DataModel a -> ChannelData DataModel a
    put = WrapDataModel

processDataModel :: AcidState DataModel.DataModel -> (a -> IO ()) -> DataModel a -> IO ()
processDataModel state return' = \case
    AddFeedIfUnique v -> Service.addFeedIfUnique state v >> return' ()
    ListFeeds -> Service.listFeeds state >>= return'

createDataModelChannel :: IO (ServiceChannel DataModel)
createDataModelChannel = createServiceChannel

runDataModel :: AcidState DataModel.DataModel -> ServiceChannel DataModel -> IO ()
runDataModel model chan = runServiceChannel chan $ processDataModel model

runDataModelEffect
    :: Member IO effs => ServiceChannel DataModel
    -> Eff (DataModel ': effs) a
    -> Eff effs a
runDataModelEffect = runServiceEffect
