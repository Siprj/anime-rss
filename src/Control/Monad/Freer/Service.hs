{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Freer.Service
    ( IscCall(..)
    , ServiceChannel
    , createServiceChannel
    , runServiceChannel
    , runServiceEffect
    , interpret
    )
  where

import Control.Applicative ((<$>), pure)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import Control.Monad ((>>=))
import Control.Monad.Freer (Eff, Member, send, handleRelay)
import Control.Monad.STM (atomically)
import Data.Function (($))
import System.IO (IO)


class IscCall service where
    data ChannelData service :: * -> *
    get :: ChannelData service a -> service a
    put :: service a -> ChannelData service a

data ChannelContainer service where
    ChannelContainer
        :: forall a service. IscCall service
        => ChannelData service a
        -> MVar a
        -> ChannelContainer service

data ServiceChannel service where
    ServiceChannel
        :: IscCall service
        => TChan (ChannelContainer service)
        -> ServiceChannel service

createServiceChannel :: IscCall service => IO (ServiceChannel service)
createServiceChannel = ServiceChannel <$> newTChanIO

runServiceChannel
    :: IscCall service
    => ServiceChannel service
    -> (forall a. (a -> IO ()) -> service a -> IO ())
    -> IO ()
runServiceChannel schan@(ServiceChannel chan) f = do
    val <- atomically $ readTChan chan
    case val of
        ChannelContainer v resMVar -> f (putMVar resMVar) $ get v
    runServiceChannel schan f

runServiceEffect
    :: forall effs service a
    . (Member IO effs, IscCall service)
    => ServiceChannel service
    -> Eff (service ': effs) a
    -> Eff effs a
runServiceEffect (ServiceChannel chan) = interpret go
  where
    go :: service s -> Eff effs s
    go v = send $ do
        respMVar <- newEmptyMVar
        atomically $ writeTChan chan (ChannelContainer (put v) respMVar)
        takeMVar respMVar

-- TODO: Move this into freer-effects!!!
interpret :: (forall s. eff s -> Eff effs s) -> Eff (eff ': effs) a -> Eff effs a
interpret f = handleRelay pure (\v f' -> f v >>= f')
