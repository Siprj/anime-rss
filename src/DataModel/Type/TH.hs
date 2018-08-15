{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DataModel.Type.TH where
#ifndef TEST
--    ( simplify
--    , TypeVariable(..)
--    )
--  where
#endif

import Prelude (error)

import Control.Applicative.Combinators (manyTill, sepBy1, many)
import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>), pure)
import Control.Lens.Setter (over)
import Control.Lens.Tuple (_1)
import Control.Monad ((>>), mapM, mapM_, when, fail)
import Data.Data (Data)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (==))
import Data.Foldable (find, length)
import Data.Function ((.), ($))
import Data.Functor (fmap, void)
import Data.List (repeat, zip, unzip, reverse, take, lookup, head)
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust, catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<))
import Data.String (String)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.IO (writeFile)
import Data.Tuple (fst)
import Data.Typeable (Typeable)
import Data.Traversable (sequence)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Parser
    (ParseResult(ParseOk, ParseFailed), parseDecl)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
    ( ConDecl(ConDecl, InfixConDecl, RecDecl)
    , Context
    , Decl(DataDecl), DataOrNew(DataType)
    , DeclHead(DHead, DHInfix, DHParen, DHApp)
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
import qualified Language.Haskell.Exts.Syntax as Exts (Name(Ident, Symbol))
import qualified Language.Haskell.TH as TH (Name)
import Language.Haskell.TH
    ( Con(NormalC, RecC, InfixC, ForallC, GadtC, RecGadtC)
    , Dec(DataD)
    , Info(TyConI)
    , Q
    , TyVarBndr(KindedTV, PlainTV)
    , mkName
    , nameBase
    , newName
    , pprint
    , reify
    , reportError
    , runIO
    )
import Language.Haskell.Meta.Syntax.Translate (toDecs)
import qualified Language.Haskell.TH as TH
    (Type(VarT, AppT, ArrowT, StarT, ConT))
import Language.Haskell.TH.Quote
    ( QuasiQuoter
        (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
    )
import Language.Haskell.TH.Syntax
    ( BangType
    , VarBangType
    )
import Text.Megaparsec.Char
    (anyChar, lowerChar, upperChar, space1, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
    ( skipBlockComment
    , skipLineComment
    , symbol
    , lexeme
    , space
    )
import Text.Megaparsec (Parsec, try, parseErrorPretty', parse)
import Text.Pretty.Simple (pShow)
import Text.Show (Show, show)


-- DataD Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]

simplify :: TH.Name -> [TypeVariable] -> String -> Q [Dec]
simplify baseTypeName substitutions newTypeName = do
    baseType <- reify baseTypeName
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
        KindedTV _ (TH.AppT (TH.AppT TH.ArrowT TH.StarT) TH.StarT) -> checkKinds xs
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
        converSub orig@(bang, TH.AppT (TH.VarT varTName) rest) =
            maybe [orig] modifyTypeVar $ findTypeVarInPairs varTName typeVarPairs
          where
            modifyTypeVar :: TypeVariable -> [BangType]
            modifyTypeVar Identity = [(bang, rest)]
            modifyTypeVar Proxy = []
            modifyTypeVar Maybe = [(bang, TH.AppT (TH.ConT $ mkName "Maybe") rest)]
            modifyTypeVar DontModify = [orig]
        converSub x = [x]

    convertConstructor (RecC name records) typeVarPairs =
        RecC (mkName $ nameBase name <> "'") . mconcat $ fmap converSub records
      where
        converSub orig@(recName, bang, TH.AppT (TH.VarT varTName) rest) =
            maybe [orig] modifyTypeVar $ findTypeVarInPairs varTName typeVarPairs
          where
            modifyTypeVar :: TypeVariable -> [VarBangType]
            modifyTypeVar Identity = [(recName, bang, rest)]
            modifyTypeVar Proxy = []
            modifyTypeVar Maybe = [(recName, bang, TH.AppT (TH.ConT $ mkName "Maybe") rest)]
            modifyTypeVar DontModify = [orig]
        converSub x = [x]
    convertConstructor constructor _ = constructor

type Name = String
type Constructor = (Name, Name)

data TypeVariable = Identity | Proxy | Maybe | DontModify
  deriving (Eq, Typeable, Generic, Data, Show)

data Alias = Alias
    { name :: Name
    , typeVariables :: [TypeVariable]
    , constructorMapping :: [Constructor]
    }

data Conversion = Conversion
    { name :: Name
    , from :: Name
    , to :: Name
    }

data Ast = Ast
    { aliases :: [Alias]
    , conversions :: [Conversion]
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
delimiter = symbol "~~~"

equal :: Parser String
equal = symbol "="

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

parseAlias :: Parser Alias
parseAlias = alias >> Alias
    <$> upperName
    <*> manyTill parseTypeVariable colon
    <*> parserConstructors

parseConversion :: Parser Conversion
parseConversion = conversion >> Conversion
    <$> (lowerName <* equal)
    <*> upperName
    <*> (arrow *> upperName)

parseAst :: Parser Ast
parseAst = makeAst . unzip <$> many (parseConversion' <|> parseAlias')
  where
    parseConversion' :: Parser (Maybe Alias, Maybe Conversion)
    parseConversion' = do
        v <- parseConversion
        pure (Nothing, Just v)

    parseAlias' :: Parser (Maybe Alias, Maybe Conversion)
    parseAlias' = do
        v <- parseAlias
        pure (Just v, Nothing)

    makeAst (a, b) = Ast (catMaybes a) (catMaybes b)

splitToHaskellStringAndAST :: Parser (String, Ast)
splitToHaskellStringAndAST = do
    haskellStr <- manyTill anyChar delimiter
    ast <- parseAst
    pure (haskellStr, ast)

quoteDec' :: String -> Q [Dec]
quoteDec' str = do
    (haskellString, ast) <- either parserFailed pure
        $ parse splitToHaskellStringAndAST "" str
    decl <- failOnError $ parseDecl haskellString
    runIO . writeFile "/tmp/pokus.txt" $ pShow decl
    generateDecl decl ast
  where
    parserFailed = fail . parseErrorPretty' str

    failOnError = \case
        ParseFailed _loc str' -> fail str'
        ParseOk v -> do
            -- reportError . unpack $ pShow v
            validateDecl v

    -- | Ensure that our declaration contains data declaration and fail
    -- otherwise.
    validateDecl :: Decl l -> Q (Decl l)
    validateDecl v@DataDecl{} = pure v
    validateDecl _ = fail "Only data declaration is supported!"

    generateDecl :: (Show l) => Decl l -> Ast -> Q [Dec]
    generateDecl (DataDecl l don cntx head' consts der) Ast{..} = do
        (newDeclHead, tvPairs) <- eitherToQ $ createNewDeclHead newName head' tv
        newConsts <- eitherToQ $ mapM (createNewConstructors tvPairs cm) consts
        pure . toDecs $ DataDecl l don cntx newDeclHead newConsts der
      where
        newName = Exts.Ident l $ (name :: Alias -> Name) $ head aliases
        tv = typeVariables $ head aliases
        cm = constructorMapping $ head aliases

myToList :: DeclHead l -> [Exts.Name l]
myToList decl =  myToList' decl []
  where
    myToList' :: DeclHead l -> [Exts.Name l] -> [Exts.Name l]
    myToList' (DHead _ _) xs = xs
    myToList' (DHInfix _ typeVar _) xs = toVarName typeVar : xs
    myToList' (DHParen _ nextDecl) xs = myToList' nextDecl xs
    myToList' (DHApp _ nextDecl typeVar) xs =
        myToList' nextDecl $ toVarName typeVar : xs

type TypeVariablePair = (String, TypeVariable)

createNewDeclHead
    :: forall l
    . Exts.Name l
    -> DeclHead l
    -> [TypeVariable]
    -> Either String (DeclHead l, [TypeVariablePair])
createNewDeclHead newName decl typeVariables =
    createNewDeclHead' decl [] $ reverse typeVariables
  where
    createNewDeclHead'
        :: DeclHead l
        -> [TypeVariablePair]
        -> [TypeVariable]
        -> Either String (DeclHead l, [TypeVariablePair])
    createNewDeclHead' (DHead l _) ys [] = Right (DHead l newName, ys)
    createNewDeclHead' (DHead _ _) _ xs =
        Left $ "Data type have to have same number of type parameters as the"
            <> " alias. Spare type variables are:\n "
            <> show (take (length xs) typeVariables)
    createNewDeclHead' v@DHInfix{} _ _ =
        Left $ "Infix type variables are not supported:\n" <> prettyPrint v
    createNewDeclHead' (DHParen l nextDecl) ys xs =
        fmap (over _1 $ DHParen l) $ createNewDeclHead' nextDecl ys xs
    createNewDeclHead' DHApp{} _ [] =
        Left $ "There is surplus of type variable in data declaration: "
            <> prettyPrint decl
    createNewDeclHead' (DHApp l nextDecl typeVar) ys (x:xs) =
        fmap (over _1 $ magic x)
            $ createNewDeclHead' nextDecl
            ((nameToStr $ toVarName typeVar, x) : ys) xs
      where
        magic :: TypeVariable -> DeclHead l -> DeclHead l
        magic DontModify v = DHApp l v typeVar
        magic _ v = v

-- TODO: Right -> pure

createNewConstructors
    :: forall l
    . [TypeVariablePair]
    -> [Constructor]
    -> QualConDecl l
    -> Either String (QualConDecl l)
createNewConstructors typeVarPairList conMappings oldCon =
    QualConDecl (annotation oldCon)
    <$> modifyTypeVar (typeVars oldCon)
    <*> Right (context oldCon)
    <*> newConDecl (conDecl oldCon)
  where
    modifyTypeVar :: Maybe [TyVarBind l] -> Either String (Maybe [TyVarBind l])
    modifyTypeVar tvs = Right tvs

    annotation :: QualConDecl l -> l
    annotation (QualConDecl l _ _ _) = l

    typeVars :: QualConDecl l -> Maybe [TyVarBind l]
    typeVars (QualConDecl _ tvs _ _) = tvs

    context :: QualConDecl l -> Maybe (Context l)
    context (QualConDecl _ _ cntx _) = cntx

    conDecl :: QualConDecl l -> ConDecl l
    conDecl (QualConDecl _ _ _ cdcl) = cdcl

    -- TODO:
    -- rmMappedTyVars

    newConDecl :: ConDecl l -> Either String (ConDecl l)
    newConDecl (ConDecl l name types) = ConDecl l
        <$> translateName name
        <*> translateTypes types
    newConDecl (InfixConDecl _ _ _ _) =
        Left "Infix data constructor is not supported."
    newConDecl (RecDecl l name records) = RecDecl l
        <$> (translateName name)
        <*> mapM processRecord records
      where
        processRecord (FieldDecl l2 names t) =
            FieldDecl l2 names <$> translateType t

    -- TODO: l type variable to ()
    translateName :: Exts.Name l -> Either String (Exts.Name l)
    translateName (Exts.Ident l name) =
        maybeToEither (nameError name) . fmap (Exts.Ident l)
        $ lookup name conMappings
    translateName (Exts.Symbol l name) =
        maybeToEither (nameError name) . fmap (Exts.Symbol l)
        $ lookup name conMappings

    nameError name = "Can't find constructor \"" <> name <> "\" in constructor "
        <> "mapping: " <> show conMappings

--    translateRecords =

    translateTypes :: [Type l] -> Either String [Type l]
    translateTypes types = sequence $ fmap translateType types

    translateType :: Type l -> Either String (Type l)
    translateType (TyForall l typeVarBind context' t) =
        TyForall l typeVarBind context' <$> translateType t
    translateType (TyFun l t1 t2) =
        TyFun l <$> translateType t1 <*> translateType t2
    translateType (TyTuple l boxed ts) =
        TyTuple l boxed <$> sequence (fmap translateType ts)
    translateType (TyUnboxedSum l ts) =
        TyUnboxedSum l <$> sequence (fmap translateType ts)
    translateType (TyList l t) =
        TyList l <$> translateType t
    translateType (TyParArray l t) =
        TyParArray l <$> translateType t
    translateType (TyApp l t1 t2) =
        case classifyLeftSide t1 of
            Keep -> TyApp l t1 <$> translateType t2
            Rename -> TyApp l (renameTyVar t2) <$> translateType t2
            GoDeeper -> TyApp l <$> translateType t1 <*> translateType t2
            Drop -> translateType t2
    translateType v@(TyVar _ name) =
        case lookup (nameToStr name) typeVarPairList of
            -- TODO: This error message sucks...
            Just _ -> Left $ "Type variable [" <> prettyPrint name
                <> "] can't be used as final type."
            Nothing -> pure v
    translateType v@TyCon{} = pure v
    translateType (TyParen l t) = TyParen l <$> translateType t
    translateType (TyInfix l t1 name t2) =
        case classifyLeftSide t1 of
            Keep -> TyInfix l t1 <$> pure name <*> translateType t2
            Rename -> TyInfix l (renameTyVar t2)
                <$> pure name
                <*> translateType t2
            GoDeeper -> TyInfix l
                <$> translateType t1
                <*> pure name
                <*> translateType t2
            Drop -> Left $ "Can't drop infix name: " <> prettyPrint name
    translateType (TyKind l t k) = TyKind l <$> translateType t <*> pure k
    -- TODO: Promoted types can contain type variables to... This mistake
    -- is multiple places.
    -- Let's ignore it for now.
    translateType v@TyPromoted{} = pure v
    -- TODO: Not sure about this one... Should it behave the same way as TyApp?
    translateType (TyEquals l t1 t2) =
        TyEquals l <$> translateType t1 <*> translateType t2
    translateType v@TySplice{} = pure v
    translateType (TyBang l bt us t) = TyBang l bt us <$> translateType t
    translateType v@TyWildCard{} = pure v
    translateType v@TyQuasiQuote{} = pure v

    classifyLeftSide :: Type l -> Classification
    classifyLeftSide TyForall{} = Keep
    classifyLeftSide TyFun{} = GoDeeper
    classifyLeftSide TyTuple{} = GoDeeper
    classifyLeftSide TyUnboxedSum{} = GoDeeper
    classifyLeftSide (TyList _ t) = classifyLeftSide t
    classifyLeftSide (TyParArray _ t) = classifyLeftSide t
    classifyLeftSide TyApp{} = GoDeeper
    classifyLeftSide (TyVar _ name) =
        case lookup (nameToStr name) typeVarPairList of
            Just DontModify -> Keep
            Just Proxy -> Drop
            Just Identity -> Rename
            Just Maybe -> Rename
            _ -> Keep
    classifyLeftSide (TyCon _ _) = Keep
    classifyLeftSide (TyParen _ t) = classifyLeftSide t
    classifyLeftSide TyInfix{} = GoDeeper
    classifyLeftSide (TyKind _ t _) = classifyLeftSide t
    classifyLeftSide (TyPromoted _ _) = Keep
    classifyLeftSide TyEquals{} = GoDeeper
    classifyLeftSide (TySplice _ _) = Keep
    classifyLeftSide (TyBang _ _ _ t) = classifyLeftSide t
    classifyLeftSide (TyWildCard _ _) = Keep
    classifyLeftSide TyQuasiQuote{} = Keep

    renameTyVar :: Type l -> Type l
    renameTyVar (TyList l t) = TyList l $ renameTyVar t
    renameTyVar (TyParArray l t) = TyParArray l $ renameTyVar t
    renameTyVar v@(TyVar l name) = maybe v toNewVaule
        $ lookup (nameToStr name) typeVarPairList
      where
        toNewVaule :: TypeVariable -> Type l
        toNewVaule Identity = TyCon l (UnQual l (Exts.Ident l "Identity"))
        toNewVaule Maybe = TyCon l (UnQual l (Exts.Ident l "Maybe"))
        -- TODO: Maybe some error handling.
        -- Following two cases should never happen...
        toNewVaule DontModify = v
        toNewVaule Proxy = v
    renameTyVar (TyParen l t) = TyParArray l $ renameTyVar t
    renameTyVar (TyKind l t k) = TyKind l (renameTyVar t) k
    renameTyVar (TyBang l bt mpn t) = TyBang l bt mpn $ renameTyVar t
    -- TODO: Maybe some error handling.
    -- Following cases should never happen...
    renameTyVar v = v

data Classification = Keep | Rename | Drop | GoDeeper

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

eitherToQ :: Either String a -> Q a
eitherToQ v = either fail (pure) v

nameToStr :: Exts.Name l -> String
nameToStr (Exts.Ident _ str) = str
nameToStr (Exts.Symbol _ str) = str

toVarName :: TyVarBind l -> Exts.Name l
toVarName (KindedVar _ name _) = name
toVarName (UnkindedVar _ name) = name

undefined :: a
undefined = undefined

genData :: QuasiQuoter
genData = QuasiQuoter
    { quoteExp = error "Usage as a expression is not supported."
    , quotePat = error "Usage as a pattern is not supported."
    , quoteType = error "Usage as a type is not supported."
    , quoteDec = quoteDec'
    }
