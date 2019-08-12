{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DataModel.Acid.User
    ( addUser
    , listUsers
    , getUser
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

import DataModel.Type.User (User, _email)
import DataModel.Type.DataModel
    ( DataModel
    , users
    )
import DataModel.Type.Feed
    ( Feed'(_url)
    , SetFeed
    , Feed
    , setFeedToFeed
    )


addUser :: AddUser -> Update DataModel (Either String Int)
addUser newUser = do
    users' <- use users
    if isJust $ find compareUserByEmail users'
        then do

        else

  where
--    prependFeed :: Vector Feed -> Vector Feed
--    prependFeed feedVector = cons (setFeedToFeed date' setFeed) feedVector

    compareUserByEmail :: Feed -> Bool
    compareUserByEmail v = _email v == _email newUser

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
