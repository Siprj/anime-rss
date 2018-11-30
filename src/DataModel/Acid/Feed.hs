{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DataModel.Acid.Feed
    ( addFeedIfUnique
    , listFeeds
    )
  where

import Control.Applicative (pure)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (Query, Update)
import Data.Eq ((==))
import Data.Function (($), (.), const)
import Data.Maybe (Maybe(Just), maybe)
import Data.Time (UTCTime)
import Data.Vector (Vector, cons, find)

import DataModel.Type.DataModel (DataModel(DataModel, feeds, lastModification))
import DataModel.Type.Feed
    ( Feed'(url)
    , SetFeed
    , Feed
    , setFeedToFeed
    )

addFeedIfUnique :: SetFeed -> UTCTime -> Update DataModel ()
addFeedIfUnique setFeed date' = do
    DataModel{..} <- get
    maybe (prependFeed feeds) (pure . const ())
        $ find compareFeeds feeds
  where
    prependFeed :: Vector Feed -> Update DataModel ()
    prependFeed feedVector = put DataModel
        { feeds = cons (setFeedToFeed date' setFeed) feedVector
        , lastModification = Just date'
        }
    compareFeeds v = url v == url setFeed

listFeeds :: Query DataModel (Vector Feed, Maybe UTCTime)
listFeeds = do
    DataModel{..} <- ask
    pure (feeds, lastModification)
