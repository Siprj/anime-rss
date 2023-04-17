module AnimeRss.Ids (
  IdKind (..),
  Id,
  unsafeId,
  fromId,
  AnimeId,
  EpisodeId,
  UserId,
  SessionId,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID, fromText, toText)
import Database.PostgreSQL.Simple.FromField qualified as SQL
import Database.PostgreSQL.Simple.ToField qualified as SQL
import Relude hiding (toText)

data IdKind
  = Anime
  | Episode
  | User
  | Session

newtype Id (a :: IdKind) = Id UUID
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, SQL.FromField, SQL.ToField)

type AnimeId = Id 'Anime

type EpisodeId = Id 'Episode

type UserId = Id 'User

type SessionId = Id 'Session

unsafeId :: Text -> Maybe (Id (a :: IdKind))
unsafeId v = Id <$> fromText v

fromId :: Id (a :: IdKind) -> Text
fromId (Id v) = toText v
