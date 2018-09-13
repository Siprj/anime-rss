{-# LANGUAGE NoImplicitPrelude #-}

module Test.DataModel.Type.TH where

import Control.Monad.Trans.Reader (Reader, ReaderT(runReaderT), ask)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.String (String)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
    ( ConDecl(ConDecl, InfixConDecl, RecDecl)
    , Context
    , Decl(DataDecl), DataOrNew(DataType)
    , DeclHead(DHead, DHInfix, DHParen, DHApp)
    , Name(Ident)
    , QualConDecl(QualConDecl)
    , TyVarBind(UnkindedVar, KindedVar)
    , Type
        ( TyApp
        , TyBang
        , TyCon
        , TyEquals
        , TyForall
        , TyFun
        , TyInfix
        , TyKind
        , TyList
        , TyParArray
        , TyParen
        , TyPromoted
        , TyQuasiQuote
        , TySplice
        , TyTuple
        , TyUnboxedSum
        , TyVar
        , TyWildCard
        )
    , QName(UnQual, Qual, Special)
    , FieldDecl(FieldDecl)
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), Assertion, testCase)

import DataModel.Type.TH
    ( TypeVariable(Proxy, Identity, DontModify, Maybe)
    , TypeVariablePair
    , createNewDeclHead
    , renameTyVar
    )


-- TODO: More complex test with declarations containing parentheses.

renameTyVarData1 :: Type ()
renameTyVarData1 = TyVar () (Ident () "a")

renameTyVarData2 :: Type ()
renameTyVarData2 = TyVar () (Ident () "b")

renameTyVarData3 :: Type ()
renameTyVarData3 = TyParen () (TyVar () (Ident () "c"))

renameTyVarData4 :: Type ()
renameTyVarData4 = TyParen () (TyVar () (Ident () "e"))

renameTyVarResult1 :: Type ()
renameTyVarResult1 = TyCon () (UnQual () (Ident () "Maybe"))

renameTyVarResult2 :: Either (Maybe String) (Type ())
renameTyVarResult2 = Left $ Just
    "Internal error `renameTyVar`. Unsupported TypeVariable constructor: Proxy"

renameTyVarResult3 :: Type ()
renameTyVarResult3 = TyParen () (TyVar () (Ident () "c"))

renameTyVarResult4 :: Either (Maybe String) (Type ())
renameTyVarResult4 = Left . Just
    $ "Internal error `renameTyVar`. Type variable is not in type variable "
    <> "list: e"

typeVariablePairs :: [TypeVariablePair]
typeVariablePairs =
    [ ("a", Maybe)
    , ("b", Proxy)
    , ("c", DontModify)
    , ("d", Identity)
    ]

renameTyVarTest1 :: Assertion
renameTyVarTest1 = Right renameTyVarResult1
    @=? runReaderT (renameTyVar renameTyVarData1) typeVariablePairs

renameTyVarTest2 :: Assertion
renameTyVarTest2 = renameTyVarResult2
    @=? runReaderT (renameTyVar renameTyVarData2) typeVariablePairs

renameTyVarTest3 :: Assertion
renameTyVarTest3 = Right renameTyVarResult3
    @=? runReaderT (renameTyVar renameTyVarData3) typeVariablePairs

renameTyVarTest4 :: Assertion
renameTyVarTest4 = renameTyVarResult4
    @=? runReaderT (renameTyVar renameTyVarData4) typeVariablePairs

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

translationList :: [TypeVariablePair]
translationList =
    [ ("a", Proxy)
    , ("b", DontModify)
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
        , testCase "renameTyVar-1" renameTyVarTest1
        , testCase "renameTyVar-2" renameTyVarTest2
        , testCase "renameTyVar-3" renameTyVarTest3
        , testCase "renameTyVar-4" renameTyVarTest4
        ]
    ]
