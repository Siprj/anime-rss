{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE TypeInType #-}

module Database.Service
    ()
  where

import Control.Applicative (pure)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM.TChan (TChan, writeTChan, readTChan)
import Control.Monad ((>>=), (>>))
import Control.Monad.Freer (Eff, Member, send, handleRelay)
import Control.Monad.STM (atomically)
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.Function (($))
import Data.Vector (Vector)
import System.IO (IO)

import Database.Model (DataModel, Feed, SetFeed, listFeeds, defaultDataModel, addFeedIfUnique)


class IscCalls service where
    data ChannelData service :: * -> *

--data ServiceConnection service where
--    ServiceConnection
--        :: Isc service
--        => TChan (ChannelData service a, MVar a)
--        -> ServiceConnection service

--class IscConnection service where
--    data ServiceConnection service :: *
--
--instance Isc service => IscConnection service where
--    data ServiceConnection service = forall a. TChan (ChannelData service a, MVar a)

-- data ServiceConnection service = forall a. ServiceConnection (TChan (ChannelData service a, MVar a))

--class (Isc service) => IscTChan service where
--    runService
--        :: ServiceConnection service
--        -> ChannelData service a -> (a -> IO ()) -> IO b
--        -> IO b
--
--    runServiceEffect
--        :: forall eff effs a. Member IO effs
--        => ServiceConnection service
--        -> Eff (ChannelData service ': effs) a
--        -> Eff effs a
--    runServiceEffect (ServiceConnection chan) = interpret go
--      where
--        go v = send $ do
--            respMVar <- newEmptyMVar
--            atomically $ writeTChan (chan :: TChan (ChannelData service a, MVar a)) (v, respMVar)
--            takeMVar respMVar

--class (IscCalls service) => IscService service where
--    data ServiceConnection service :: *



undef :: a
undef = undef

-- instance (IscCalls service) => IscService service where
    -- data ServiceConnection service = TChan (ChannelData service a, MVar a)
runService
    :: DatabaseConnection
    -> (forall a. (a -> IO ()) -> Database a -> IO b)
    -> IO b
runService chan f = do
    val <- atomically $ readTChan chan
    case val of
        Dd (v, resMVar)-> f (putMVar resMVar) v
    runService chan f

runServiceEffect
    :: forall eff effs a s. Member IO effs
    => DatabaseConnection
    -> Eff (Database ': effs) a
    -> Eff effs a
runServiceEffect chan = interpret go
  where
    go :: forall s effs. Member IO effs => Database s -> Eff effs s
    go v = send $ do
        respMVar <- newEmptyMVar
        atomically $ writeTChan chan (Dd (v, respMVar))
        takeMVar respMVar

data Dd = forall a. Dd (Database a, MVar a)
type DatabaseConnection = TChan Dd

data Database s where
    AddFeedIfUnique :: SetFeed -> Database ()
    ListFeeds :: Database (Vector Feed)

runDatabase :: AcidState DataModel -> (a -> IO ()) -> Database a -> IO ()
runDatabase state return' = \case
    AddFeedIfUnique v -> addFeedIfUnique state v >> return' ()
    ListFeeds -> listFeeds state >>= return'
--syncCall :: (eff s -> IO a) -> IO a
--syncCall =

interpret :: (forall s. eff s -> Eff effs s) -> Eff (eff ': effs) a -> Eff effs a
interpret f = handleRelay pure (\v f' -> f v >>= f')

--runDatabase :: Member IO r => TChan (Database s) -> Eff (Database ': r) w -> Eff r w
--runDatabase = interpret go
