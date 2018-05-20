{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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

import Control.Applicative ((<$>), pure)
import Control.Monad ((>>), mapM_, when)
import Data.Eq ((==))
import Data.Foldable (find, length)
import Data.Function ((.), ($))
import Data.Functor (fmap)
import Data.List (repeat, zip)
import Data.Maybe (Maybe, maybe, isJust)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<))
import Data.String (String)
import Data.Tuple (fst)
import Language.Haskell.TH
    ( Con(NormalC, RecC, InfixC, ForallC, GadtC, RecGadtC)
    , Dec(DataD)
    , Info(TyConI)
    , Name
    , Q
    , Type(VarT, AppT, ArrowT, StarT, ConT)
    , TyVarBndr(KindedTV, PlainTV)
    , reify
    , reportError
    , runIO
    , mkName
    , newName
    , nameBase
    , pprint
    )
import Language.Haskell.TH.Syntax
    ( BangType
    , VarBangType
    )
import System.IO (writeFile)
import Text.Show (show)


data TypeVariable = Identity | Proxy | Maybe | DontModify

-- DataD Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]

simplify :: Name -> [TypeVariable] -> String -> Q [Dec]
simplify baseTypeName substitutions newTypeName = do
    baseType <- reify baseTypeName
    runIO . writeFile "/tmp/pokus" $ show baseType
    case baseType of
        TyConI (DataD cxt _ typeVariabls kind constructors derivings) -> do
            when (isJust kind) $ reportError
                "Kinded data contractor is not currently supported"
            when (length typeVariabls < length substitutions) . reportError
                $ "Data type " <> show baseTypeName
                <> " don't has enough type variables."

            let typeVariablePairs =
                    zip (substitutions <> repeat DontModify) typeVariabls
            -- TODO: Is this behaviour intuitive???
            checkKinds typeVariablePairs
            mapM_ checkConstructor constructors
            newName' <- newName newTypeName
            let newConstructors =
                    fmap (`convertConstructor` typeVariablePairs) constructors
            pure [DataD cxt newName' typeVariabls kind newConstructors derivings]
        -- TODO: Better error message needed!
        _ -> reportError
            "Currently only \"plain data constructors\" are supported."
            >> pure []
  where
    checkKinds :: [(TypeVariable, TyVarBndr)] -> Q ()
    checkKinds ((DontModify, _):xs) = checkKinds xs
    checkKinds ((_, typeVarBndr):xs) = case typeVarBndr of
        PlainTV _ -> checkKinds xs
        -- Represents kind * -> *
        KindedTV _ (AppT (AppT ArrowT StarT) StarT) -> checkKinds xs
        KindedTV name kind -> reportError $ "Type variable: " <> pprint name
            <> " has kind: " <> pprint kind
            <> "\n Expected kind is: * -> *."
    checkKinds [] = pure ()

    checkConstructor (InfixC _ name _) = reportError
        $ "Wrong constructor: " <> pprint name
        <> "Infix constructors are not supported."
    checkConstructor ForallC{} = reportError
        "Forall constructors are not supported."
    checkConstructor GadtC{} = reportError
        "GADT constructors are not supported."
    checkConstructor RecGadtC{} = reportError
        "GADT constructors are not supported."
    checkConstructor _ = pure ()

    findTypeVarInPairs
        :: Name
        -> [(TypeVariable, TyVarBndr)]
        -> Maybe TypeVariable
    findTypeVarInPairs name list = fst <$> find (go name) list
      where
        go name' (_, PlainTV name'') = name' == name''
        go name' (_, KindedTV name'' _) = name' == name''

    convertConstructor :: Con -> [(TypeVariable, TyVarBndr)] -> Con
    convertConstructor (NormalC name subs) typeVarPairs =
        NormalC (mkName $ nameBase name <> "'") . mconcat $ fmap converSub subs --
      where
        converSub orig@(bang, AppT (VarT varTName) rest) =
            maybe [orig] modifyTypeVar $ findTypeVarInPairs varTName typeVarPairs
          where
            modifyTypeVar :: TypeVariable -> [BangType]
            modifyTypeVar Identity = [(bang, rest)]
            modifyTypeVar Proxy = []
            modifyTypeVar Maybe = [(bang, AppT (ConT $ mkName "Maybe") rest)]
            modifyTypeVar DontModify = [orig]
        converSub x = [x]

    convertConstructor (RecC name records) typeVarPairs =
        RecC (mkName $ nameBase name <> "'") . mconcat $ fmap converSub records
      where
        converSub orig@(recName, bang, AppT (VarT varTName) rest) =
            maybe [orig] modifyTypeVar $ findTypeVarInPairs varTName typeVarPairs
          where
            modifyTypeVar :: TypeVariable -> [VarBangType]
            modifyTypeVar Identity = [(recName, bang, rest)]
            modifyTypeVar Proxy = []
            modifyTypeVar Maybe = [(recName, bang, AppT (ConT $ mkName "Maybe") rest)]
            modifyTypeVar DontModify = [orig]
        converSub x = [x]
    convertConstructor constructor _ = constructor

