{-# LANGUAGE NoImplicitPrelude #-}

module Test.DataModel.Type.TH where

import Data.Either (Either(Left, Right))
import Language.Haskell.Exts.Syntax
    ( DeclHead(DHead, DHApp)
    , TyVarBind(UnkindedVar)
    , Name(Ident)
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), Assertion, testCase)

import DataModel.Type.TH
    (TypeVariable(Proxy, Identity, DontModify, Maybe), createNewDeclHead)


-- TODO: More complex test with declarations containing parentheses.

dataDecl :: DeclHead ()
dataDecl = DHApp ()
    ( DHApp ()
        (DHead () (Ident () "C"))
        (UnkindedVar () (Ident () "a"))
    )
    (UnkindedVar () (Ident () "b"))

newDataDecl :: DeclHead ()
newDataDecl = DHApp ()
    (DHead () newName)
    (UnkindedVar () (Ident () "b"))

newName :: Name ()
newName = Ident () "NewName"

typeVarList :: [TypeVariable]
typeVarList = [Proxy, DontModify]

longTypeVarList :: [TypeVariable]
longTypeVarList = [DontModify, DontModify, Identity]

shortTypeVarList :: [TypeVariable]
shortTypeVarList =  [Maybe]

translationList :: [(Name (), TypeVariable)]
translationList =
    [ (Ident () "a", Proxy)
    , (Ident () "b", DontModify)
    ]

testShort :: Assertion
testShort = Left "There is surplus of type variable in data declaration: C a b"
    @=? createNewDeclHead newName dataDecl shortTypeVarList

testLong :: Assertion
testLong = Left "Data type have to have same number of type parameters as the \
    \alias. Spare type variables are:\n [DontModify]"
    @=? createNewDeclHead newName dataDecl longTypeVarList

successfulTranslationTest :: Assertion
successfulTranslationTest = Right (newDataDecl, translationList)
    @=? createNewDeclHead newName dataDecl typeVarList


tests :: TestTree
tests = testGroup "DataModel.Type.TH"
    [ testGroup "createNewDeclHead"
        [ testCase "long-type-variable-list" testLong
        , testCase "shot-type-variable-list" testShort
        , testCase "successful-translation" successfulTranslationTest
        ]
    ]
