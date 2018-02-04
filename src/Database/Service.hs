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

module Database.Service
    ( Database
    , addFeedIfUnique
    , createDatabaseChannel
    , listFeeds
    , runDatabase
    , runDatabaseEffect
    )
  where

import Control.Monad ((>>=), (>>))
import Control.Monad.Freer (Eff, Member, send)
import Data.Acid (AcidState)
import Data.Function (($), (.))
import Data.Vector (Vector)
import System.IO (IO)

import Database.Model (DataModel, Feed, SetFeed)
import qualified Database.Model as DataModel(listFeeds, addFeedIfUnique)
import Control.Monad.Freer.Service
    ( IscCall(ChannelData, get, put)
    , ServiceChannel
    , createServiceChannel
    , runServiceChannel
    , runServiceEffect
    )


data Database s where
    AddFeedIfUnique :: SetFeed -> Database ()
    ListFeeds :: Database (Vector Feed)

addFeedIfUnique :: Member Database effs => SetFeed -> Eff effs ()
addFeedIfUnique = send . AddFeedIfUnique

listFeeds :: Member Database effs => Eff effs (Vector Feed)
listFeeds = send ListFeeds

instance IscCall Database where
    data ChannelData Database a = WrapDatabase (Database a)

    get :: ChannelData Database a -> Database a
    get (WrapDatabase v) = v

    put :: Database a -> ChannelData Database a
    put = WrapDatabase

processDatabase :: AcidState DataModel -> (a -> IO ()) -> Database a -> IO ()
processDatabase state return' = \case
    AddFeedIfUnique v -> DataModel.addFeedIfUnique state v >> return' ()
    ListFeeds -> DataModel.listFeeds state >>= return'

createDatabaseChannel :: IO (ServiceChannel Database)
createDatabaseChannel = createServiceChannel

runDatabase :: AcidState DataModel -> ServiceChannel Database -> IO ()
runDatabase model chan = runServiceChannel chan $ processDatabase model

runDatabaseEffect
    :: Member IO effs => ServiceChannel Database
    -> Eff (Database ': effs) a
    -> Eff effs a
runDatabaseEffect = runServiceEffect
