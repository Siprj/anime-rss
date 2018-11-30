{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module DataModel.Type.Apply
    ( Apply
    , ApplyType(..)
    )
  where

import Data.Maybe (Maybe)
import Data.Kind (Type)

data ApplyType
    = Drop
    | Optional
    | Keep

type family Apply (a :: ApplyType) (b :: Type) where
    Apply 'Drop _ = ()
    Apply 'Optional b = Maybe b
    Apply 'Keep b = b
