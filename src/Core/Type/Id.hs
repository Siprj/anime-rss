{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
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

import Data.Int (Int64)
import Text.Show (Show)
import Data.Eq (Eq)


data IdKind
    = Anime
    | Episode
    | User

newtype Id (a :: IdKind) = Id Int64
  deriving (Show, Eq)

type AnimeId = Id 'Anime
type EpisodeId = Id 'Episode
type UserId = Id 'User

unsafeId :: Int64 -> Id (a :: IdKind)
unsafeId = Id

fromId :: Id (a :: IdKind) -> Int64
fromId (Id v) = v
