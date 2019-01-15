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

import Control.Applicative ((<*>))
import Control.Lens
    ( (%=)
    , (.=)
    , ReifiedGetter(Getter)
    , runGetter
    , use
    , view
    )
import Control.Monad (when)
import Data.Acid (Query, Update)
import Data.Bool (Bool)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just), isJust)
import Data.Time (UTCTime)
import Data.Vector (Vector, cons, find)

import DataModel.Type.DataModel
    ( DataModel
    , feeds
    , lastFeedsModification
    )
import DataModel.Type.Feed
    ( Feed'(_url)
    , SetFeed
    , Feed
    , setFeedToFeed
    )


addFeedIfUnique :: SetFeed -> UTCTime -> Update DataModel ()
addFeedIfUnique setFeed date' = do
    feeds' <- use feeds
    when (isJust $ find compareFeeds feeds') $ do
        feeds %= prependFeed
        lastFeedsModification .= Just date'
  where
    prependFeed :: Vector Feed -> Vector Feed
    prependFeed feedVector = cons (setFeedToFeed date' setFeed) feedVector

    compareFeeds :: Feed -> Bool
    compareFeeds v = _url v == _url setFeed

listFeeds :: Query DataModel (Vector Feed, Maybe UTCTime)
listFeeds = do
    view . runGetter $ (,) <$> Getter feeds <*> Getter lastFeedsModification
