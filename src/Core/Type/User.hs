{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Type.User
    ( User(..)
    , NewUser(..)
    , Email
    )
  where

import Core.Type.Id (UserId)
import Data.Text (Text)
import Data.UUID (UUID)
import Optics (makeFieldLabelsWith, noPrefixFieldLabels)
import Text.Show (Show)

import Crypto.PasswordStore (PasswordHash)


type Email = Text

data User = User
    { userId ::UserId
    , email :: Email
    , name :: Text
    , password :: PasswordHash
    , episodeChannel :: UUID
    }
  deriving (Show)

data NewUser = NewUser
    { email :: Email
    , name :: Text
    , password :: PasswordHash
    }
  deriving (Show)

makeFieldLabelsWith noPrefixFieldLabels ''User
makeFieldLabelsWith noPrefixFieldLabels ''NewUser
