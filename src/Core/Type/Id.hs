{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Core.Type.Id
    ( Id(..)
    , AnimeId
    , EpisodeId
    , UserId
    , unsafeId
    , fromId
    )
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq)
import Data.Int (Int64)
import Text.Show (Show)


data IdKind
    = Anime
    | Episode
    | User

newtype Id (a :: IdKind) = Id Int64
  deriving (Show, Eq)
  deriving newtype (FromJSON, ToJSON)

type AnimeId = Id 'Anime
type EpisodeId = Id 'Episode
type UserId = Id 'User

unsafeId :: Int64 -> Id (a :: IdKind)
unsafeId = Id

fromId :: Id (a :: IdKind) -> Int64
fromId (Id v) = v
