{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Type.Episode
    ( Episode(..)
    )
  where

import Core.Type.Id (AnimeId)
import Data.Int (Int)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Text.Show (Show)

data Episode = Episode
    { episodeId :: AnimeId
    , title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    , number :: Int
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Episode
