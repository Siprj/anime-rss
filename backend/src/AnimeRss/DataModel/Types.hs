{-# LANGUAGE DeriveAnyClass #-}

module AnimeRss.DataModel.Types
  ( Episode (..)
  , CreateEpisode (..)
  , CreateUser (..)
  , User (..)
  , TemporaryKey (..)
  , Anime (..)
  , CreateUserFollows (..)
  , DeleteUserFollows (..)
  , Error (..)
  , unexpectedAmountOfResults
  , unexpectedAmountOfActions
  , DbPasswordHash(..)
  , toPasswordHash
  , UserRelatedAnime(..)
  )
where

import AnimeRss.Ids
import AnimeRss.Url
import Control.Exception
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Database.PostgreSQL.Simple as SQL
import Relude hiding (id)
import Crypto.PasswordStore
import qualified Database.PostgreSQL.Simple.ToField as SQL
import Data.Serialize
import qualified Database.PostgreSQL.Simple.FromField as SQL hiding (Binary)

data Error
  = UnexpectedAmountOfActions UnexpectedAmountOfActionsData
  | UnexpectedAmountOfResults UnexpectedAmountOfResultsData
  deriving stock (Show)

instance Exception Error

data UnexpectedAmountOfActionsData = UnexpectedAmountOfActionsData
  { origin :: Text
  , count :: Int64
  , expectedMininumCount :: Int64
  , expectedMaximumCount :: Int64
  }
  deriving stock (Show, Generic)

unexpectedAmountOfActions :: Text -> Int64 -> Int64 -> Int64 -> Error
unexpectedAmountOfActions origin expectedMininumCount expectedMaximumCount count = UnexpectedAmountOfActions $ UnexpectedAmountOfActionsData {..}

data UnexpectedAmountOfResultsData = UnexpectedAmountOfResultsData
  { origin :: Text
  , count :: Int
  , expectedMininumCount :: Int
  , expectedMaximumCount :: Int
  }
  deriving stock (Show, Generic)

unexpectedAmountOfResults :: Text -> Int -> Int -> Int -> Error
unexpectedAmountOfResults origin expectedMininumCount expectedMaximumCount count = UnexpectedAmountOfResults $ UnexpectedAmountOfResultsData {..}

data Episode = Episode
  { title :: Text
  , url :: Url
  , number :: Text
  , imageUrl :: Url
  , date :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (SQL.FromRow)

data CreateEpisode = CreateEpisode
  { url :: Url
  , title :: Text
  , number :: Text
  , imageUrl :: Url
  , animeUrl :: Url
  }
  deriving stock (Show, Generic)

data CreateUser = CreateUser
  { name :: Text
  , email :: Text
  , password :: DbPasswordHash
  }
  deriving stock (Show, Generic)

data User = User
  { id :: UserId
  , name :: Text
  , email :: Text
  , password :: DbPasswordHash
  , newsChannel :: UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (SQL.FromRow)

data TemporaryKey = TemporaryKey
  { key :: TemporaryKeyId
  , until :: UTCTime
  , userId :: UserId
  , userEmail :: Text
  }
  deriving stock (Show, Generic)

data Anime = Anime
  { id :: AnimeId
  , title :: Text
  , imgUrl :: Text
  , url :: Text
  , date :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (SQL.FromRow)

data CreateUserFollows = CreateUserFollows
  { userId :: UserId
  , animeId :: AnimeId
  }
  deriving stock (Show, Generic)

data DeleteUserFollows = DeleteUserFollows
  { userId :: UserId
  , animeId :: AnimeId
  }
  deriving stock (Show, Generic)

data DbPasswordHash = DbPasswordHash
  { password :: ByteString
  , hashParameters :: HashParameters
  }
  deriving stock (Show, Generic)
  deriving anyclass (Serialize)

toPasswordHash :: DbPasswordHash -> PasswordHash
toPasswordHash DbPasswordHash{..} = (hashParameters, password)

instance SQL.ToField DbPasswordHash where
  toField = SQL.toField . SQL.Binary . encode

instance SQL.FromField DbPasswordHash where
  fromField :: SQL.FieldParser DbPasswordHash
  fromField field mdata = SQL.fromField field mdata
    >>= (either mkError pure . decode . SQL.fromBinary)
    where
      mkError :: String -> SQL.Conversion DbPasswordHash
      mkError = SQL.returnError SQL.ConversionFailed field

data UserRelatedAnime = UserRelatedAnime
    { animeId :: AnimeId
    , title :: Text
    , url :: Text
    , imageUrl :: Text
    , date :: UTCTime
    , following :: Bool
    }
  deriving stock (Show, Generic)
  deriving anyclass (SQL.FromRow)
