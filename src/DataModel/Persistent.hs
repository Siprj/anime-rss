{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DataModel.Persistent
    ( FeedId
    , Feed(..)
    , UserId
    , User(..)
    , EntityField(..)
    , migrateAll
    )
  where

import Crypto.PasswordStore (PasswordHash)
import Data.Int (Int)
import Database.Persist.TH
    ( mkMigrate
    , share
    , mkPersist
    , persistLowerCase
    , sqlSettings
    )
import Database.Persist.Class (EntityField)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Text.Show (Show)
import Network.URI (URI)
import DataModel.OrphanInstances ()


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Feed
    name Text
    url URI
    imgUrl URI
    episodeNumber Int
    date UTCTime

    UniqueUserName url
    deriving Show

User
    name Text
    email Text
    password PasswordHash

    UniqueUsername email
    deriving Show
|]

