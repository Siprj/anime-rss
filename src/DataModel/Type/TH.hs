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

import Control.Monad ((>>), when, unless)
import Control.Applicative (pure)
import Data.Bool (Bool(True, False))
import Data.Ord ((<))
import Data.Monoid ((<>))
import Data.String (String)
import Data.Foldable (length)
import Data.Function ((.), ($))
import Language.Haskell.TH (Q, TyVarBndr(KindedTV), Info(TyConI), Dec(DataD), Name, reify, runIO, reportError)
import System.IO (writeFile)
import Text.Show (show)


data TypeVariable = Identity | Proxy | Maybe | TypeVariable

simplify :: Name -> [TypeVariable] -> String -> Q [Dec]
simplify baseTypeName substitutions _newName = do
    baseType <- reify baseTypeName
    runIO . writeFile "/tmp/pokus" $ show baseType
    case baseType of
        TyConI (DataD _cxt _ typeVariabls _kind _constructor _derivings) -> do
            checkTypeVariables typeVariabls
            pure []
        -- TODO: Better error message needed!
        _ -> reportError
            "Currently only \"plain data constructors\" are supported." >> pure []
  where
    checkTypeVariables typeVariabls = do
        when (length typeVariabls < length substitutions)
            . reportError $ "Data type " <> show baseTypeName
            <> " don't has enought type variables."
        unless (isWellKinded typeVariabls substitutions)
            . reportError $ "BLABLA"

    isWellKinded ((KindedTV _ _):xs) (TypeVariable:ys) = isWellKinded xs ys
    isWellKinded ((KindedTV _ _):_) _ = False
    isWellKinded (_:xs) (_:ys) = isWellKinded xs ys
    isWellKinded [] _ = True
    isWellKinded _ [] = True
