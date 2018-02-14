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
import Data.Int (Int)
import Data.Maybe (fromJust, maybe)
import Data.SafeCopy
    ( Migrate(MigrateFrom, migrate)
    , base
    , deriveSafeCopy
    , extension
    )
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

data Feed_v0 = Feed_v0
    { name_v0 :: Text
    , url_v0 :: URI
    , date_v0 :: UTCTime
    , imgUrl_v0 :: URI
    }
  deriving (Eq, Typeable, Generic, Data, Show)

data Feed = Feed
    { name :: Text
    , url :: URI
    , date :: UTCTime
    , imgUrl :: URI
    , episodeNumber :: Int
    }
  deriving (Eq, Typeable, Generic, Data, Show)

instance Migrate Feed where
    type MigrateFrom Feed = Feed_v0
    migrate Feed_v0{..} = Feed
        { name = name_v0
        , url = url_v0
        , imgUrl = imgUrl_v0
        , date = date_v0
        , episodeNumber = 0
        }

$(deriveSafeCopy 0 'base ''Feed_v0)
$(deriveSafeCopy 1 'extension ''Feed)
$(deriveSafeCopy 0 'base ''DataModel)

data SetFeed_v0 = SetFeed_v0
    { setFeedName_v0 :: Text
    , setFeedUrl_v0 :: URI
    , setFeedImgUrl_v0 :: URI
    }

data SetFeed = SetFeed
    { setFeedName :: Text
    , setFeedUrl :: URI
    , setFeedImgUrl :: URI
    , setFeedEpisodeNumber :: Int
    }

instance Migrate SetFeed where
    type MigrateFrom SetFeed = SetFeed_v0
    migrate SetFeed_v0{..} = SetFeed
        { setFeedName = setFeedName_v0
        , setFeedUrl = setFeedUrl_v0
        , setFeedImgUrl = setFeedImgUrl_v0
        , setFeedEpisodeNumber = 0
        }

$(deriveSafeCopy 0 'base ''SetFeed_v0)
$(deriveSafeCopy 1 'extension ''SetFeed)

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
        , episodeNumber = setFeedEpisodeNumber
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
