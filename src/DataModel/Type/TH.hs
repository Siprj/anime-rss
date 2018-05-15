
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.TH
    ( simplify
    , TypeVariable(..)
    )
  where

import Control.Monad ((>>))
import Control.Applicative (pure)
import Data.String (String)
import Data.Function ((.), ($))
import Language.Haskell.TH (Q, Info(TyConI), Dec(DataD), Name, reify, runIO, reportError)
import System.IO (writeFile)
import Text.Show (show)


data TypeVariable = Identity | Proxy | Maybe

simplify :: Name -> [TypeVariable] -> String -> Q [Dec]
simplify baseTypeName _typeVariables _newName = do
    baseType <- reify baseTypeName
    runIO . writeFile "/tmp/pokus" $ show baseType
    case baseType of
        TyConI (DataD _cxt _ _typeVariabls' _kind _constructor _derivings) ->
            pure []
        -- TODO: Better error message needed!
        _ -> reportError
            "Currently only \"plain data constructors\" are supported." >> pure []
--  where
--    checkTypeParameters typeVariabls' typeVariables=
