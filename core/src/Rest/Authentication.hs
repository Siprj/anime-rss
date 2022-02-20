{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rest.Authentication
    ( UserAuthentication
    )
  where

import Servant.API (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)

import Core.Type.User (User)

type instance AuthServerData (AuthProtect "cookie-auth") = User

type UserAuthentication = AuthProtect "cookie-auth"
