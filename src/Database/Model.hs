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
    , SetFeed(..)
    , Feed(..)
    , addFeedIfUnique
    , defaultDataModel
    , listFeeds
    )
  where

import Control.Applicative (pure)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, makeAcidic, query, update)
import Data.Bool (Bool(False))
import Data.Data (Data)
import Data.Eq ((==), Eq)
import Data.Function (($), (.), const)
import Data.Maybe (fromJust, maybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , getCurrentTime
    , parseTimeM
    )
import Data.Typeable (Typeable)
import Data.Vector (Vector, cons, find, fromList)
import GHC.Generics (Generic)
import Network.URI (URI)
import System.IO (IO)
import Text.Show (Show)

import Database.OrphanInstances ()


data DataModel = DataModel
    { feeds :: Vector Feed
    , lastModification :: UTCTime
    }

defaultTime :: UTCTime
defaultTime = fromJust $ parseTimeM False defaultTimeLocale "%s" "0"

defaultDataModel :: DataModel
defaultDataModel = DataModel (fromList []) defaultTime

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
    , setFeedImgUrl :: URI
    }

$(deriveSafeCopy 0 'base ''SetFeed)

addFeedIfUnique' :: SetFeed -> UTCTime -> Update DataModel ()
addFeedIfUnique' SetFeed{..} date = do
    DataModel{..} <- get
    maybe (prependFeed feeds) (pure . const ())
        $ find compareFeeds feeds
  where
    prependFeed :: Vector Feed -> Update DataModel ()
    prependFeed feedVector = put $ DataModel
        { feeds = cons buildFeed feedVector
        , lastModification = date
        }
    buildFeed = Feed
        { name = setFeedName
        , url = setFeedUrl
        , date
        , imgUrl = setFeedImgUrl
        }
    compareFeeds Feed{url} = setFeedUrl == url

listFeeds' :: Query DataModel (Vector Feed, UTCTime)
listFeeds' = do
    DataModel{..} <- ask
    pure (feeds, lastModification)

$(makeAcidic ''DataModel ['addFeedIfUnique', 'listFeeds'])

addFeedIfUnique :: AcidState DataModel -> SetFeed -> IO ()
addFeedIfUnique state feed = do
    now <- getCurrentTime
    update state (AddFeedIfUnique' feed now)

listFeeds :: AcidState DataModel -> IO (Vector Feed, UTCTime)
listFeeds state = query state ListFeeds'
