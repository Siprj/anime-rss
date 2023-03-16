{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module DataModel.Queries
    ( GetEpisode(..)
    , CreateEpisode(..)
    , createDbUser
    )
  where

import Data.UUID (UUID)
import GHC.Generics (Generic)
import Network.URI (URI)
import Data.Text (Text)
import Data.Time (UTCTime)
import DataModel.Persistent (AnimeId)
import GHC.Show (Show)
import Data.ByteString (ByteString)
import qualified Core.Type.User as Core
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Connection, query)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (($))
import qualified Core.DataModel.Type.Error as DataModel (Error(..))

data GetEpisode = GetEpisode
    { id :: UUID
    , url :: URI
    , number :: Text
    , date :: UTCTime
    , animeId :: AnimeId
    }
  deriving stock (Show, Generic)

data CreateEpisode = CreateEpisode
    { url :: URI
    , number :: Text
    , date :: UTCTime
    , animeId :: AnimeId
    }
  deriving stock (Show, Generic)

-- createDbUser :: MonadIO m => Core.NewUser -> m User
-- createDbUser Core.NewUser{..} = do
--     episodeChannel <- liftIO nextRandom
--     pure $ User
--         { userName = name
--         , userEmail = email
--         , userPassword = password
--         , userNewEpisodeChannel = episodeChannel
--         }

createDbUser :: MonadIO m => Connection -> Core.NewUser -> m Either DataModel.Error Core.User
createDbUser connection Core.NewUser{..} = do
    results <- liftIO $ query connection [sql| INSERT INTO users (
            name,
            email,
            password
        )
        VALUES ()
        RETURNING
            id,
            name,
            email,
            new_episode_channel,
            password
        |] (name, email, password)


data CreateUser = CreateUser
    { name :: Text
    , email :: Text
    , password :: ByteString
    , newsChannel :: UUID
    }
  deriving stock (Show, Generic)

data GetUser = GetUser
    { id :: UUID
    , name :: Text
    , email :: Text
    , password :: ByteString
    , newsChannel :: UUID
    }
  deriving stock (Show, Generic)

data TemporaryKey = TemporaryKey
   { key :: UUID
   , until :: UTCTime
   , userId :: UUID
   , userEmail :: Text
   }
  deriving stock (Show, Generic)

data CreateAnime = CreateAnime
    { title :: Text
    , imgUrl :: Text
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)

data GetAnime = GetAnime
    { id :: UUID
    , title :: Text
    , imgUrl :: Text
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)

data UserFollows = UserFollows
    { userId :: UUID
    , animeId :: UUID
    , url :: Text
    , date :: UTCTime
    }
  deriving stock (Show, Generic)


