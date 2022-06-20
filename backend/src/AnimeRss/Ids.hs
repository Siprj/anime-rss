module AnimeRss.Ids
  ( IdKind (..)
  , Id
  , unsafeId
  , fromId
  , AnimeId
  , EpisodeId
  , UserId
  , TemporaryKeyId
  )
where

import Relude hiding (toText)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID, fromText, toText)
import qualified Database.PostgreSQL.Simple.FromField as SQL
import qualified Database.PostgreSQL.Simple.ToField as SQL

data IdKind
  = Anime
  | Episode
  | User
  | TemporaryKey

newtype Id (a :: IdKind) = Id UUID
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, SQL.FromField, SQL.ToField)

type AnimeId = Id 'Anime

type EpisodeId = Id 'Episode

type UserId = Id 'User

type TemporaryKeyId = Id 'TemporaryKey

unsafeId :: Text -> Maybe (Id (a :: IdKind))
unsafeId v = Id <$> fromText v

fromId :: Id (a :: IdKind) -> Text
fromId (Id v) = toText v
