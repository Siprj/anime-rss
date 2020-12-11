{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DataModel.Persistent
    ( EpisodeId
    , Episode(..)
    , UserId
    , User(..)
    , EntityField(..)
    , AnimeId
    , Anime(..)
    , UsersAnime(..)
    , UsersAnimeId
    , TemporaryKeyId
    , TemporaryKey(..)
    , Unique(..)
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
import Database.Persist.Class (EntityField, Unique)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Text.Show (Show)
import Network.URI (URI)
import DataModel.OrphanInstances ()
import Data.UUID (UUID)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
    url URI
    number Int
    date UTCTime
    animeId AnimeId

    UniqueUrl url
    UniqueTitleAndNumber animeId number

    deriving Show

User
    name Text
    email Text
    password PasswordHash
    newEpisodeChannel UUID

    UniqueUserEmail email

    deriving Show

TemporaryKey
    key UUID
    until UTCTime
    userEmail Text

    deriving Show

Anime
    title Text
    imgUrl URI
    animeUrl URI
    date UTCTime

    UniqueAnimeTitle title
    UniqueAnimeUrl animeUrl

    deriving Show

UsersAnime
    userEmail Text
    animeTitle Text
    follow Text

    deriving Show

|]

