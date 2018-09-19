{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.DataModel.Type.TH where

import Control.Exception (SomeException, try)
import Control.Monad ((>>=))
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
import Language.Haskell.TH (runQ)
import System.IO (IO)
import Test.Tasty.HUnit ((@=?), Assertion, testCase, assertFailure)
import Test.Tasty (TestTree, testGroup)
import Text.Show (show)

import DataModel.Type.TH
    ( Alias(Alias)
    , Ast(Ast)
    , TypeVariable(Proxy, Identity, DontModify, Maybe)
    , TypeVariablePair
    , createNewDeclHead
    , renameTyVar
    , translateType
    , checkAstForDuplicities
    )


-- TODO: More complex test with declarations containing parentheses.

typeVariablePairs :: [TypeVariablePair]
typeVariablePairs =
    [ ("a", Maybe)
    , ("b", Proxy)
    , ("c", DontModify)
    , ("d", Identity)
    ]

renameTyVarData1 :: Type ()
renameTyVarData1 = TyVar () (Ident () "a")

renameTyVarResult1 :: Type ()
renameTyVarResult1 = TyCon () (UnQual () (Ident () "Maybe"))

renameTyVarTest1 :: Assertion
renameTyVarTest1 = Right renameTyVarResult1
    @=? runReaderT (renameTyVar renameTyVarData1) typeVariablePairs

renameTyVarData2 :: Type ()
renameTyVarData2 = TyVar () (Ident () "b")

renameTyVarResult2 :: Either (Maybe String) (Type ())
renameTyVarResult2 = Left $ Just
    "Internal error `renameTyVar`. Unsupported TypeVariable constructor: Proxy"

renameTyVarTest2 :: Assertion
renameTyVarTest2 = renameTyVarResult2
    @=? runReaderT (renameTyVar renameTyVarData2) typeVariablePairs

renameTyVarResult3 :: Type ()
renameTyVarResult3 = TyParen () (TyVar () (Ident () "c"))

renameTyVarData3 :: Type ()
renameTyVarData3 = TyParen () (TyVar () (Ident () "c"))

renameTyVarTest3 :: Assertion
renameTyVarTest3 = Right renameTyVarResult3
    @=? runReaderT (renameTyVar renameTyVarData3) typeVariablePairs

renameTyVarResult4 :: Either (Maybe String) (Type ())
renameTyVarResult4 = Left . Just
    $ "Internal error `renameTyVar`. Type variable is not in type variable "
    <> "list: e"

renameTyVarData4 :: Type ()
renameTyVarData4 = TyParen () (TyVar () (Ident () "e"))

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

translateTypeData1 :: Type ()
translateTypeData1 = TyApp ()
    (TyVar () (Ident () "b" )) (TyCon () (UnQual () (Ident () "Text")))

translateTypeResult1 :: Either (Maybe String) (Type ())
translateTypeResult1 = Left Nothing

translateTypeTest1 :: Assertion
translateTypeTest1 = translateTypeResult1
    @=? runReaderT (translateType translateTypeData1) typeVariablePairs

translateTypeData2 :: Type ()
translateTypeData2 = TyApp ()
    (TyVar () (Ident () "a"))
    (TyParen ()
        (TyFun () (TyVar () (Ident () "e")) (TyVar () (Ident () "e")))
    )

translateTypeResult2 :: Either (Maybe String) (Type ())
translateTypeResult2 = Right $ TyApp ()
        ( TyCon () (UnQual () (Ident () "Maybe")))
        ( TyParen ()
            (TyFun () (TyVar () (Ident () "e")) (TyVar () (Ident () "e")))
        )

translateTypeTest2 :: Assertion
translateTypeTest2 = translateTypeResult2
    @=? runReaderT (translateType translateTypeData2) typeVariablePairs

translateTypeData3 :: Type ()
translateTypeData3 = TyApp ()
    (TyVar () (Ident () "a" )) (TyCon () (UnQual () (Ident () "Text")))

translateTypeResult3 :: Either (Maybe String) (Type ())
translateTypeResult3 = Right $ TyApp ()
    (TyCon () (UnQual () (Ident () "Maybe")))
    (TyCon () (UnQual () (Ident () "Text")))

translateTypeTest3 :: Assertion
translateTypeTest3 = translateTypeResult3
    @=? runReaderT (translateType translateTypeData3) typeVariablePairs

translateTypeData4 :: Type ()
translateTypeData4 = TyApp ()
    (TyVar () (Ident () "d" )) (TyCon () (UnQual () (Ident () "Text")))

translateTypeResult4 :: Either (Maybe String) (Type ())
translateTypeResult4 = Right $ TyCon () (UnQual () (Ident () "Text"))

translateTypeTest4 :: Assertion
translateTypeTest4 = translateTypeResult4
    @=? runReaderT (translateType translateTypeData4) typeVariablePairs

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

duplicities1 :: Ast
duplicities1 = Ast [Alias "Foo" "Bar" [] [], Alias "Foo" "Bar" [] []] []

duplicitiesResult1 :: Either String ()
duplicitiesResult1 = Left "Following aliases names are duplicated: Foo"

testDuplicities1 :: Assertion
testDuplicities1 = duplicitiesResult1 @=? checkAstForDuplicities duplicities1

duplicities2 :: Ast
duplicities2 = Ast [Alias "Foo" "Bar" [] [("Asdf", "Kwa"), ("Asdf", "Kwa2")]] []

duplicitiesResult2 :: Either String ()
duplicitiesResult2 =
    Left "Following constructors are mapped to multiple targets: Asdf"

testDuplicities2 :: Assertion
testDuplicities2 = duplicitiesResult2 @=? checkAstForDuplicities duplicities2

duplicities3 :: Ast
duplicities3 = Ast [Alias "Foo" "Bar" [] [("Asdf", "Kwa"), ("Asdf2", "Kwa")]] []

duplicitiesResult3 :: Either String ()
duplicitiesResult3 =
    Left "Following constructors names are duplicated: Kwa"

testDuplicities3 :: Assertion
testDuplicities3 = duplicitiesResult3 @=? checkAstForDuplicities duplicities3

duplicities4 :: Ast
duplicities4 = Ast
    [ Alias "Foo" "Bar" [Identity] [("Asdf", "Kwa2"), ("Asdf2", "Kwa1")]
    , Alias "Foo2" "Bar" [Proxy] [("Asdf", "Kwa1"), ("Asdf2", "Kwa3")]
    ]
    []

duplicitiesResult4 :: Either String ()
duplicitiesResult4 =
    Left "Following constructors names are duplicated: Kwa1"

testDuplicities4 :: Assertion
testDuplicities4 = duplicitiesResult4 @=? checkAstForDuplicities duplicities4

duplicities5 :: Ast
duplicities5 = Ast
    [ Alias "Foo" "Bar" [Proxy] [("Asdf", "Kwa1"), ("Asdf2", "Kwa2")]
    , Alias "Foo2" "Bar" [Proxy] [("Asdf", "Kwa3"), ("Asdf2", "Kwa4")]
    ]
    []

duplicitiesResult5 :: Either String ()
duplicitiesResult5 = Right ()

testDuplicities5 :: Assertion
testDuplicities5 = duplicitiesResult5 @=? checkAstForDuplicities duplicities5

duplicities6 :: Ast
duplicities6 = Ast
    [ Alias "Foo" "Bar" [Identity] [("Asdf", "Kwa1"), ("Asdf2", "Kwa2")]
    , Alias "Foo2" "Bar" [Proxy] [("Asdf", "Kwa3"), ("Asdf2", "Kwa4")]
    ]
    []

duplicitiesResult6 :: Either String ()
duplicitiesResult6 = Right ()

testDuplicities6 :: Assertion
testDuplicities6 = duplicitiesResult6 @=? checkAstForDuplicities duplicities6

-- TODO: Tests for pairDeclAndAliases

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
        , testCase "translateType-1" translateTypeTest1
        , testCase "translateType-2" translateTypeTest2
        , testCase "translateType-3" translateTypeTest3
        , testCase "translateType-4" translateTypeTest4
        , testCase "duplicities-1" testDuplicities1
        , testCase "duplicities-2" testDuplicities2
        , testCase "duplicities-3" testDuplicities3
        , testCase "duplicities-4" testDuplicities4
        , testCase "duplicities-5" testDuplicities5
        , testCase "duplicities-6" testDuplicities6
        ]
    ]
