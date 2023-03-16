{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module DataModel.Queries
    ( Error(..)
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

data Error
    = RowCountMissmatch RowCountMissmatchT
    |
  deriving stock (Show, Generic)

data RowCountMissmatchT = RowCountMissmatchT {}
