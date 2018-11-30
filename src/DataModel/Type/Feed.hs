{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataModel.Type.Feed
    ( Feed'(..)
    , Feed
    , SetFeed
    , setFeedToFeed
    )
  where

import Control.Applicative ((<$>), (<*>))
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.SafeCopy
    ( Migrate(MigrateFrom, migrate)
    , SafeCopy(putCopy, getCopy, kind, version)
    , contain
    , extension
    , safeGet
    , safePut
    )
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.URI (URI)
import Text.Show (Show)

import DataModel.Type.Apply (Apply, ApplyType(Drop, Keep))
import DataModel.OrphanInstances ()
import DataModel.Type.Old.Feed
    ( Feed_v1(Feed_v1, name_v1, url_v1, date_v1, imgUrl_v1, episodeNumber_v1)
    , SetFeed_v1(SetFeed_v1, setFeedName_v1, setFeedUrl_v1, setFeedImgUrl_v1, setFeedEpisodeNumber_v1)
    )


data Feed' a b = Feed'
    { name :: Apply a Text
    , url :: Apply a URI
    , imgUrl :: Apply a URI
    , episodeNumber :: Apply a Int
    , date :: Apply b UTCTime
    }
  deriving (Generic, Typeable)

type SetFeed = Feed' 'Keep 'Drop

deriving instance Show SetFeed
deriving instance Eq SetFeed
deriving instance Data SetFeed

instance SafeCopy SetFeed where
    version = 2
    kind = extension
    putCopy Feed'{..} = contain $ do
        safePut name
        safePut url
        safePut imgUrl
        safePut episodeNumber
        safePut date

    getCopy = contain $ Feed'
        <$> safeGet
        <*> safeGet
        <*> safeGet
        <*> safeGet
        <*> safeGet

instance Migrate SetFeed where
    type MigrateFrom SetFeed = SetFeed_v1
    migrate SetFeed_v1{..} = Feed'
        { name = setFeedName_v1
        , url = setFeedUrl_v1
        , imgUrl = setFeedImgUrl_v1
        , episodeNumber = setFeedEpisodeNumber_v1
        , date = ()
        }

type Feed = Feed' 'Keep 'Keep

deriving instance Show Feed
deriving instance Eq Feed
deriving instance Data Feed

instance Migrate Feed where
    type MigrateFrom Feed = Feed_v1
    migrate Feed_v1{..} = Feed'
        { name = name_v1
        , url = url_v1
        , imgUrl = imgUrl_v1
        , episodeNumber = episodeNumber_v1
        , date = date_v1
        }

instance SafeCopy Feed where
    version = 2
    kind = extension
    putCopy Feed'{..} = contain $ do
        safePut name
        safePut url
        safePut imgUrl
        safePut episodeNumber
        safePut date

    getCopy = contain $ Feed'
        <$> safeGet
        <*> safeGet
        <*> safeGet
        <*> safeGet
        <*> safeGet

setFeedToFeed :: UTCTime -> SetFeed -> Feed
setFeedToFeed date' v = v{date = date'}
