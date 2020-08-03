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
    , AnimeFollowedByUserId
    , AnimeFollowedByUser(..)
    , migrateAll
    )
  where

import Crypto.PasswordStore (PasswordHash)
import Data.Bool (Bool)
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
import Data.UUID (UUID)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
    url URI
    number Int
    date UTCTime
    animeTitle Text
    imgUrl URI

    UniqueUrl url
    UniqueTitleAndNumber animeTitle number

    deriving Show

User
    name Text
    email Text
    password PasswordHash
    newAnimeChannel UUID
    newEpisodeChannel UUID

    UniqueUserEmail email

    deriving Show

TemporaryKey
    key UUID
    until UTCTime
    userEmail Text

    deriving Show

AnimeFollowedByUser
    userEmail Text
    animeTitle Text
    follows Bool

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

    deriving Show

|]
