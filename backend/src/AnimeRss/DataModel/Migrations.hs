{-# LANGUAGE QuasiQuotes #-}

module AnimeRss.DataModel.Migrations
  (migrateAll
  , initTemporaryKeysTableChange)
where

import Relude hiding (id)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Drifter (Change (Change, changeDependencies, changeDescription, changeMethod, changeName), ChangeName (ChangeName, changeNameText))
import Drifter.PostgreSQL (Method (MigrationQuery), PGMigration, runMigrations)
import Database.PostgreSQL.Simple
import Control.Monad.Catch
import Prelude (Show(show))

newtype MigrationError = MigrationError String
instance Show MigrationError where
  show (MigrationError str) = str
instance Exception MigrationError

migrateAll :: Connection -> IO ()
migrateAll connection = runMigrations connection migrationList >>= either (throwM . MigrationError) pure
  where
    migrationList =
      [ enableUUIDChange
      , initEpisodesTableChange
      , initUsersTableChange
--      , initTemporaryKeysTableChange
      , initAnimesTableChange
      , initUserFollowsTableChange
      ]

enableUUIDMigrationName :: ChangeName
enableUUIDMigrationName = ChangeName { changeNameText = "enable_uuid" }

enableUUIDChange :: Change PGMigration
enableUUIDChange =
  Change
    { changeName = enableUUIDMigrationName
    , changeDescription = Just "Enable UUID so we can start using them in tables."
    , changeDependencies = []
    , changeMethod =
        MigrationQuery
          [sql| create extension "uuid-ossp" |]
    }

initEpisodesMigrationName :: ChangeName
initEpisodesMigrationName = ChangeName {changeNameText = "init_episodes"}

initEpisodesTableChange :: Change PGMigration
initEpisodesTableChange =
  Change
    { changeName = initEpisodesMigrationName
    , changeDescription = Just "Create episodes table."
    , changeDependencies = [enableUUIDMigrationName, initAnimesMigrationName]
    , changeMethod =
        MigrationQuery
          [sql| CREATE TABLE episodes (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        url TEXT NOT NULL UNIQUE,
        number REAL NOT NULL,
        date TIMESTAMP WITH TIME ZONE DEFAULT (now() AT TIME ZONE('utc')),
        anime_id UUID NOT NULL,
        FOREIGN KEY (anime_id) REFERENCES animes(id) ON DELETE CASCADE,
        UNIQUE (anime_id, number)
        )|]
    }

initUsersMigrationName :: ChangeName
initUsersMigrationName = ChangeName {changeNameText = "init_users"}

initUsersTableChange :: Change PGMigration
initUsersTableChange =
  Change
    { changeName = initUsersMigrationName
    , changeDescription = Just "Create users table."
    , changeDependencies = [enableUUIDMigrationName]
    , changeMethod =
        MigrationQuery
          [sql| CREATE TABLE users (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        name TEXT NOT NULL ,
        email TEXT NOT NULL UNIQUE,
        news_channel UUID DEFAULT uuid_generate_v4(),
        password BYTEA NOT NULL
        )|]
    }

initTemporaryKeysMigrationName :: ChangeName
initTemporaryKeysMigrationName = ChangeName {changeNameText = "init_temporary_keys"}

initTemporaryKeysTableChange :: Change PGMigration
initTemporaryKeysTableChange =
  Change
    { changeName = initTemporaryKeysMigrationName
    , changeDescription = Just "Create temporary_keys table."
    , changeDependencies = [enableUUIDMigrationName, initUsersMigrationName]
    , changeMethod =
        MigrationQuery
          [sql| CREATE TABLE temporary_keys (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        user_id UUID NOT NULL,
        key UUID NOT NULL,
        until TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
        )|]
    }

initAnimesMigrationName :: ChangeName
initAnimesMigrationName = ChangeName {changeNameText = "init_animes"}

initAnimesTableChange :: Change PGMigration
initAnimesTableChange =
  Change
    { changeName = initAnimesMigrationName
    , changeDescription = Just "Create animes table."
    , changeDependencies = [enableUUIDMigrationName]
    , changeMethod =
        MigrationQuery
          [sql| CREATE TABLE animes (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        title TEXT NOT NULL UNIQUE,
        image_url TEXT NOT NULL,
        url TEXT NOT NULL UNIQUE,
        date TIMESTAMP WITH TIME ZONE DEFAULT (now() AT TIME ZONE('utc'))
        )|]
    }

initUserFollowsMigrationName :: ChangeName
initUserFollowsMigrationName = ChangeName {changeNameText = "init_user_follows"}

initUserFollowsTableChange :: Change PGMigration
initUserFollowsTableChange =
  Change
    { changeName = initUserFollowsMigrationName
    , changeDescription = Just "Create user_follows stable."
    , changeDependencies = [enableUUIDMigrationName, initUsersMigrationName]
    , changeMethod =
        MigrationQuery
          [sql| CREATE TABLE user_follows (
        id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
        user_id UUID NOT NULL,
        image_url TEXT NOT NULL,
        url TEXT NOT NULL UNIQUE,
        date TIMESTAMP WITH TIME ZONE DEFAULT (now() AT TIME ZONE('utc')),
        FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
        )|]
    }
