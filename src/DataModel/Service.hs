{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad ((>>=), void)
import Control.Monad.Freer (Eff, Member, send)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)
import Data.Function (($), (.))
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe, listToMaybe)
import Data.Time (UTCTime)
import System.IO (IO)
import Database.Persist
    ( SelectOpt(Desc, LimitTo)
    , Entity(entityVal)
    , insertUniqueEntity
    , selectList
    )
import Database.Persist.Sql (SqlBackend, runMigration)
import Database.Persist.Sqlite (runSqlite)

import Control.Monad.Freer.Service
    ( IscCall(ChannelData, get, put)
    , ServiceChannel
    , createServiceChannel
    , runServiceChannel
    , runServiceEffect
    )
import DataModel.Persistent (Feed(feedDate), EntityField(FeedDate), migrateAll)


data DataModel s where
    AddFeedIfUnique :: Feed -> DataModel ()
    ListFeeds :: DataModel ([Feed], Maybe UTCTime)

addFeedIfUnique :: Member DataModel effs => Feed -> Eff effs ()
addFeedIfUnique = send . AddFeedIfUnique

listFeeds :: Member DataModel effs => Eff effs ([Feed], Maybe UTCTime)
listFeeds = send ListFeeds

instance IscCall DataModel where
    data ChannelData DataModel a = WrapDataModel (DataModel a)

    get :: ChannelData DataModel a -> DataModel a
    get (WrapDataModel v) = v

    put :: DataModel a -> ChannelData DataModel a
    put = WrapDataModel

type DataModelMonad = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

processDataModel :: (a -> DataModelMonad ()) -> DataModel a -> DataModelMonad ()
processDataModel return' = \case
    AddFeedIfUnique v -> (void $ insertUniqueEntity v) >>= return'
    ListFeeds -> do
        feeds <- (fmap entityVal) <$> selectList [] [Desc FeedDate, LimitTo 50]
        return' (feeds, feedDate <$> listToMaybe feeds)

createDataModelChannel :: IO (ServiceChannel DataModel)
createDataModelChannel = createServiceChannel

-- TODO: Get the connection string as argument.
runDataModel :: ServiceChannel DataModel -> IO ()
runDataModel chan = runSqlite ":memory:" $ do
    runMigration migrateAll
    runServiceChannel chan $ processDataModel

runDataModelEffect
    :: Member IO effs => ServiceChannel DataModel
    -> Eff (DataModel ': effs) a
    -> Eff effs a
runDataModelEffect = runServiceEffect
