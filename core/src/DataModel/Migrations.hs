{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DataModel.Migrations
    ( migrateAll
    )
  where

import Drifter (Change(Change, changeName, changeDependencies, changeDescription, changeMethod), ChangeName(changeNameText, ChangeName), migrate)
import Drifter.PostgreSQL (Method(MigrationQuery), PGMigration, DBConnection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Maybe (Maybe(Nothing))
import System.IO (IO)
import Data.Either (Either)
import Data.String (String)


migrateAll :: DBConnection PGMigration -> IO (Either String ())
migrateAll connection = migrate connection migrationList
  where
    migrationList =
        [ initEpisodesTableChange
        , initUsersTableChange
        , initTemporaryKeysTableChange
        , initAnimesTableChange
        , initUserFollowsTableChange
        ]

initEpisodesMigrationName :: ChangeName
initEpisodesMigrationName = ChangeName { changeNameText = "init_episodes" }

initEpisodesTableChange :: Change PGMigration
initEpisodesTableChange = Change
    { changeName = initEpisodesMigrationName
    , changeDescription = Nothing
    , changeDependencies = []
    , changeMethod = MigrationQuery [sql| CREATE TABLE episodes (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        url TEXT NOT NULL UNIQUE,
        number REAL NOT NULL UNIQUE,
        date TIMESTAMP NOT NULL,
        anime_id UUI NOT NULLD,
        FOREIGN KEY anime_id REFERENCES animes id ON DELETE CASCADE
        )|]
    }

initUsersMigrationName :: ChangeName
initUsersMigrationName = ChangeName { changeNameText = "init_users" }

initUsersTableChange :: Change PGMigration
initUsersTableChange = Change
    { changeName = initUsersMigrationName
    , changeDescription = Nothing
    , changeDependencies = []
    , changeMethod = MigrationQuery [sql| CREATE TABLE users (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        name TEXT NOT NULL ,
        email TEXT NOT NULL UNIQUE,
        new_episode_channel UUID DEFAULT uuid_generate_v4(),
        password BYTEA NOT NULL
        )|]
    }

initTemporaryKeysMigrationChange :: ChangeName
initTemporaryKeysMigrationChange = ChangeName { changeNameText = "init_temporary_keys" }

initTemporaryKeysTableChange :: Change PGMigration
initTemporaryKeysTableChange = Change
    { changeName = initTemporaryKeysMigrationChange
    , changeDescription = Nothing
    , changeDependencies = []
    , changeMethod = MigrationQuery [sql| CREATE TABLE temporary_keys (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        user_id UUID NOT NULL,
        key UUID NOT NULL,
        until TIMESTAMP,
        FOREIGN KEY id REFERENCES users ON DELETE CASCADE
        )|]
    }

initAnimesMigrationChange :: ChangeName
initAnimesMigrationChange = ChangeName { changeNameText = "init_animes" }

initAnimesTableChange :: Change PGMigration
initAnimesTableChange = Change
    { changeName = initAnimesMigrationChange
    , changeDescription = Nothing
    , changeDependencies = []
    , changeMethod = MigrationQuery [sql| CREATE TABLE animes (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        title TEXT NOT NULL UNIQUE,
        image_url TEXT NOT NULL,
        url TEXT NOT NULL UNIQUE,
        date TIMESTAMP NOT NULL,
        )|]
    }

initUserFollowsMigrationChange :: ChangeName
initUserFollowsMigrationChange = ChangeName { changeNameText = "init_user_follows" }

initUserFollowsTableChange :: Change PGMigration
initUserFollowsTableChange = Change
    { changeName = initUserFollowsMigrationChange
    , changeDescription = Nothing
    , changeDependencies = []
    , changeMethod = MigrationQuery [sql| CREATE TABLE user_follows (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        user_id UUID NOT NULL,
        image_url TEXT NOT NULL,
        url TEXT NOT NULL UNIQUE,
        date TIMESTAMP NOT NULL,
        FOREIGN KEY user_id REFERENCES users id ON DELETE CASCADE
        )|]
    }
