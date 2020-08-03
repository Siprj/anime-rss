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
    )
  where

import Core.Type.Id (AnimeId)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)
import Text.Show (Show)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)


data Anime = Anime
    { animeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Anime
