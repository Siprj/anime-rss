{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Type.EpisodeEntry
    ( EpisodeEntry(..)
    )
  where

import Data.Text (Text)
import Network.URI (URI)
import Text.Show (Show)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)


data EpisodeEntry = EpisodeEntry
    { title :: Text
    , url :: URI
    , imageUrl :: URI
    , animeUrl :: URI
    , number :: Text
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''EpisodeEntry
