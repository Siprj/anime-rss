{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Model
    ( DataModel
    , SetFeed
    , Feed(..)
    , addFeedIfUnique
    , listFeeds
    )
  where

import Control.Applicative (pure)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.Data (Data)
import Data.Eq ((==), Eq)
import Data.Function (($), (.), const)
import Data.Maybe (maybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector, cons, find)
import GHC.Generics (Generic)
import Network.URI (URI)
import System.IO (IO)
import Text.Show (Show)

import Database.OrphanInstances ()


data DataModel = DataModel (Vector Feed)

data Feed = Feed
    { name :: Text
    , url :: URI
    , date :: UTCTime
    , imgUrl :: URI
    }
  deriving (Eq, Typeable, Generic, Data, Show)

$(deriveSafeCopy 0 'base ''Feed)
$(deriveSafeCopy 0 'base ''DataModel)

data SetFeed = SetFeed
    { setFeedName :: Text
    , setFeedUrl :: URI
    , setFeedDate :: UTCTime
    , setFeedImgUrl :: URI
    }

$(deriveSafeCopy 0 'base ''SetFeed)

addFeedIfUnique' :: SetFeed -> UTCTime -> Update DataModel ()
addFeedIfUnique' SetFeed{..} date = do
    DataModel feedVector <- get
    maybe (prependFeed feedVector) (pure . const ())
        $ find compareFeeds feedVector
  where
    prependFeed :: Vector Feed -> Update DataModel ()
    prependFeed feedVector = put $ DataModel $ cons buildFeed feedVector
    buildFeed = Feed
        { name = setFeedName
        , url = setFeedUrl
        , date
        , imgUrl = setFeedImgUrl
        }
    compareFeeds Feed{url} = setFeedUrl == url

listFeeds' :: Query DataModel (Vector Feed)
listFeeds' = do
    DataModel feedVector <- ask
    pure feedVector

$(makeAcidic ''DataModel ['addFeedIfUnique', 'listFeeds'])

addFeedIfUnique :: AcidState DataModel -> SetFeed -> IO ()
addFeedIfUnique state feed = do
    now <- getCurrentTime
    update state (AddFeedIfUnique' feed now)

listFeeds :: AcidState DataModel -> IO (Vector Feed)
listFeeds state = do
    query state ListFeeds'
