{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.TH where
--    ( simplify
--    , TypeVariable(..)
--    )
--  where

import Prelude (error)

import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>), (<|>), (*>), pure)
import Control.Applicative.Combinators (manyTill, sepBy1, many)
import Control.Monad ((>>), mapM_, when)
import Data.Eq (Eq, (==))
import Data.Foldable (find, length)
import Data.Function ((.), ($))
import Data.Functor (fmap, void)
import Data.List (repeat, zip)
import Data.Maybe (Maybe, maybe, isJust)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<))
import Data.String (String)
import Data.Void (Void)
import Data.Tuple (fst)
import Language.Haskell.Exts.Parser
    (ParseResult(ParseOk, ParseFailed), parseDecl)
import qualified Language.Haskell.TH as TH (Name)
import Language.Haskell.TH
    ( Con(NormalC, RecC, InfixC, ForallC, GadtC, RecGadtC)
    , Dec(DataD)
    , Info(TyConI)
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
import Language.Haskell.TH.Quote
    ( QuasiQuoter
        (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
    )
import Language.Haskell.TH.Syntax
    ( BangType
    , VarBangType
    )
import System.IO (writeFile)
import Text.Show (Show, show)
import Text.Megaparsec (Parsec, try)
import Text.Megaparsec.Char
    (anyChar, eol, lowerChar, upperChar, space1, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
    ( skipBlockComment
    , skipLineComment
    , symbol
    , lexeme
    , space
    )


data TypeVariable = Identity | Proxy | Maybe | DontModify
  deriving (Eq, Typeable, Generic, Data, Show)

-- DataD Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]

simplify :: TH.Name -> [TypeVariable] -> String -> Q [Dec]
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
            pure [DataD cxt newName' typeVariabls kind newConstructors
                derivings]
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
        :: TH.Name
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

type Name = String
type Asts = [Ast]
type Constructor = (Name, Name)

data Ast
    = Alias
        { name :: Name
        , typeVariables :: [TypeVariable]
        , constructorMapping :: [Constructor]
        }
    | Conversion
        { name :: Name
        , from :: Name
        , to :: Name
        }

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

alias :: Parser String
alias = symbol "alias"

conversion :: Parser String
conversion = symbol "conversion"

pipe :: Parser String
pipe = symbol "|"

colon :: Parser String
colon = symbol ":"

arrow :: Parser String
arrow = symbol "->"

thickArrow :: Parser String
thickArrow = symbol "=>"

delimiter :: Parser String
delimiter = symbol "~~~" >> eol

reservedWords :: [String]
reservedWords =
    ["if","then","else","case","type","skip","true","false","not","and","or"]

upperName :: Parser String
upperName = (lexeme . try) p
  where
    p = (:) <$> upperChar <*> many alphaNumChar

lowerName :: Parser String
lowerName = (lexeme . try) p
  where
    p = (:) <$> lowerChar <*> many alphaNumChar

parserConstructorMapping :: Parser Constructor
parserConstructorMapping = do
    left <- upperName
    void thickArrow
    right <- upperName
    pure (left, right)

parserConstructors :: Parser [Constructor]
parserConstructors = sepBy1 parserConstructorMapping pipe

parseTypeVariable :: Parser TypeVariable
parseTypeVariable = (try (symbol "Identity") >> pure Identity)
    <|> (try (symbol "Proxy") >> pure Proxy)
    <|> (try (symbol "Maybe") >> pure Maybe)
    <|> (try (symbol "DontModify") >> pure DontModify)

parseAlias :: Parser Ast
parseAlias = alias >> Alias
    <$> upperName
    <*> manyTill parseTypeVariable colon
    <*> parserConstructors

parseConversion :: Parser Ast
parseConversion = conversion >> Conversion
    <$> lowerName
    <*> upperName
    <*> (arrow *> upperName )

parseAst :: Parser [Ast]
parseAst = many (parseConversion <|> parseAlias)

splitToHaskellStringAndAST :: Parser (String, Ast)
splitToHaskellStringAndAST = do
    haskellStr <- manyTill anyChar delimiter
    ast <- parseAst
    pure (haskellStr, ast)

quoteExp :: String -> Q Exp
quoteExp str = do
    (haskellString, ast) <- either parserFailed pure
        $ parse splitToHaskellStringAndAST "" str
    decl <- parseDecl haskellString >>= \case
        ParseFailed loc str -> reportError $ str
        ParseOk v -> v
  where
    parserFailed = parseErrorPretty' str

    -- TODO: ....

--dataGen' :: String -> Q Exp
--dataGen' str =

undefined :: a
undefined = undefined

genData :: QuasiQuoter
genData = QuasiQuoter
    { quoteExp = undefined
    , quotePat = error "Usage as a parttern is not supported."
    , quoteType = error "Usage as a type is not supported."
    , quoteDec = error "Usage as a declaration is not supported."
    }
