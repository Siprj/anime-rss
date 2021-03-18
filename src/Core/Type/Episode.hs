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

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.URI (URI)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Text.Show (Show)

data Episode = Episode
    { title :: Text
    , url :: URI
    , imageUrl :: URI
    , date :: UTCTime
    , number :: Text
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''Episode
