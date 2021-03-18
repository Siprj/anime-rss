{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Type.UserFollow
    ( UserFollow(..)
    )
  where

import Core.Type.Id (UserId, AnimeId)
import Data.Bool (Bool)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Text.Show (Show)


data UserFollow = UserFollow
    { userId :: UserId
    , animeId :: AnimeId
    , follow :: Bool
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''UserFollow
