{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Type.Anime
    ( Anime(..)
    , UserRelatedAnime(..)
    )
  where

import Core.Type.Id (AnimeId)
import Data.Bool (Bool)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Text.Show (Show)


data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    }
  deriving (Show)

data UserRelatedAnime = UserRelatedAnime
    { animeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    , following :: Bool
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Anime
makeFieldLabelsWith noPrefixFieldLabels ''UserRelatedAnime
