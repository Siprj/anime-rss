{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AnimeRss.Rest.Authentication (
  UserAuthentication,
) where

import AnimeRss.DataModel.Types (User)
import Servant.API (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData)

type instance AuthServerData (AuthProtect "cookie-auth") = User

type UserAuthentication = AuthProtect "cookie-auth"
