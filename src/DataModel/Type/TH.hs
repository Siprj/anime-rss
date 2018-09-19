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
{-# LANGUAGE TupleSections #-}

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
import Control.Monad ((>>=), (>>), mapM, mapM_, when, fail)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT(runReaderT), ask)
import Data.Data (Data)
import Data.Bool (Bool, otherwise)
import Data.Either (Either(Left, Right), either)
import Data.Eq (Eq, (==), (/=))
import Data.Foldable (find, length)
import Data.Function ((.), ($), const)
import Data.Functor (fmap, void)
import Data.List
    ( (\\)
    , elem
    , head
    , intercalate
    , length
    , lookup
    , nub
    , repeat
    , reverse
    , take
    , unzip
    , zip
    )
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust, catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.Ord ((<), compare)
import Data.String (String)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.IO (writeFile)
import Data.Tuple (fst, snd, uncurry)
import Data.Typeable (Typeable)
import Data.Traversable (sequence)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Parser
    (ParseResult(ParseOk, ParseFailed), parseDecl, parseModule)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
    ( Module(Module, XmlPage, XmlHybrid)
    , ConDecl(ConDecl, InfixConDecl, RecDecl)
    , Context
    , Decl(DataDecl), DataOrNew(DataType)
    , DeclHead(DHead, DHInfix, DHParen, DHApp)
    , QualConDecl(QualConDecl)
    , TyVarBind(UnkindedVar, KindedVar)
    , Deriving
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
    ( Dec
    , Q
    , mkName
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
import qualified System.IO as S (writeFile)


type Name = String
type Constructor = (Name, Name)

data TypeVariable = Identity | Proxy | Maybe | DontModify
  deriving (Eq, Typeable, Generic, Data, Show)

data Alias = Alias
    { newName :: Name
    , originalName :: Name
    , typeVariables :: [TypeVariable]
    , constructorMapping :: [Constructor]
    }
  deriving (Show)

data Conversion = Conversion
    { name :: Name
    , from :: Name
    , to :: Name
    }
  deriving (Show)

data Ast = Ast
    { aliases :: [Alias]
    , conversions :: [Conversion]
    }
  deriving (Show)

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

upperName :: Parser Name
upperName = (lexeme . try) p
  where
    p = (:) <$> upperChar <*> many alphaNumChar

lowerName :: Parser Name
lowerName = (lexeme . try) p
  where
    p = (:) <$> lowerChar <*> many alphaNumChar

parseOriginalName :: Parser Name
parseOriginalName = equal *> upperName

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
    <*> parseOriginalName
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

data DataDec = DataDec
    { dataOrNew :: DataOrNew ()
    , context :: Maybe (Context ())
    , declarationHead :: DeclHead ()
    , constructorDeclarations :: [QualConDecl ()]
    , derivings :: [Deriving ()]
    }
  deriving (Show, Eq)

toDataDec :: Decl () -> Q DataDec
toDataDec (DataDecl _ dataOrNew context declHead const derivings) =
    pure $ DataDec
        { dataOrNew
        , context
        , declarationHead = declHead
        , constructorDeclarations = const
        , derivings
        }
toDataDec _ = fail "Only data/newtype declaration are supported!"

-- TODO: Remove runIO . writeFile ...
quoteDec' :: String -> Q [Dec]
quoteDec' str = do
    (haskellString, ast) <- either parserFailed pure
        $ parse splitToHaskellStringAndAST "" str
    decls <- toDecls (parseModule haskellString)
        >>= mapM (toDataDec . fmap (const ()))

    failOnConversionFunctions ast
    eitherToQ $ checkAstForDuplicities ast
    pairs <- eitherToQ $ pairDeclAndAliases decls $ aliases ast

    runIO . writeFile "/tmp/pokus.txt" $ pShow decls
    ret <- mconcat <$> mapM (uncurry generateDataDec) pairs
    runIO . writeFile "/tmp/pokus2.txt" $ pShow ret
--    pure ret
    pure []
  where
    parserFailed = fail . parseErrorPretty' str

    failOnConversionFunctions Ast{..} = when (length conversions /= 0)
        $ fail "Conversion function generation not yet supported."

    toDecls = \case
        ParseFailed _loc str' -> fail str'
        ParseOk (Module _ _ _ _ decls) -> pure decls
        ParseOk _ -> fail "Not a valid haskell code."

    generateDataDec :: Alias -> DataDec -> Q [Dec]
    generateDataDec Alias {..} (DataDec don cntx head' consts der) = do
        (newDeclHead, tvPairs) <-
            eitherToQ $ createNewDeclHead newName' head' typeVariables
        newConsts <- eitherToQ
            $ mapM (createNewConstructors tvPairs constructorMapping) consts

        runIO . writeFile "/tmp/pokus3.txt" $ pShow $ fmap (\_-> ()) $ DataDecl () don cntx newDeclHead newConsts der
        pure . toDecs $ DataDecl () don cntx newDeclHead newConsts der
      where
        -- TODO: Pair aliases and Decls. Head in following lines is wrong.
        newName' = Exts.Ident () newName

checkAstForDuplicities :: Ast -> Either String ()
checkAstForDuplicities Ast{..} = do
    when (length repeatedNewNames /= 0) newNameRepetitionError
    when (length repeatedNewConstNames /= 0) newConstNameRepetitionError
    when (length repeatedOrigConstNames /= 0) origConstNameRepetitionError
    -- TODO: Test for conversion functions.
  where
    repeatedNewNames = snd . repeated $ fmap newName aliases

    repeatedNewConstNames = snd . repeated .  mconcat
        $ fmap (fmap snd . constructorMapping) aliases

    repeatedOrigConstNames = mconcat $
        fmap (snd . repeated . fmap fst . constructorMapping) aliases

    newConstNameRepetitionError :: Either String ()
    newConstNameRepetitionError =
        Left $ "Following constructors names are duplicated: "
        <> intercalate ", " repeatedNewConstNames

    origConstNameRepetitionError :: Either String ()
    origConstNameRepetitionError =
        Left $ "Following constructors are mapped to multiple targets: "
        <> intercalate ", " repeatedOrigConstNames

    newNameRepetitionError :: Either String ()
    newNameRepetitionError =
        Left $ "Following aliases names are duplicated: "
        <> intercalate ", " repeatedNewNames

repeatedBy :: (a -> a -> Bool) -> [a] -> ([a], [a])
repeatedBy f = both reverse . go ([], [])
  where
    go (occurred, repeated') [] = (occurred, repeated')
    go (occurred, repeated') (x:xs)
        | isJust $ find (f x) repeated' = go (occurred, repeated') xs
        | isJust $ find (f x) occurred = go (occurred, x:repeated') xs
        | otherwise = go (x:occurred, repeated') xs

    both f (a, b) = (f a, f b)

repeated :: Eq a => [a] -> ([a], [a])
repeated = both reverse . go ([], [])
  where
    go (occurred, repeated') [] = (occurred, repeated')
    go (occurred, repeated') (x:xs)
        | x `elem` repeated' = go (occurred, repeated') xs
        | x `elem` occurred = go (occurred, x:repeated') xs
        | otherwise = go (x:occurred, repeated') xs

    both f (a, b) = (f a, f b)

pairDeclAndAliases :: [DataDec] -> [Alias] -> Either String [(Alias, DataDec)]
pairDeclAndAliases decls aliases = do
    xs <- pairedAliases

    if length (ocuredDecls xs) == length decls
        then pairedAliases
        else errDataDecl $ decls \\ ocuredDecls xs
  where
    pairedAliases = mapM pairAlias aliases
    pairAlias a@Alias{..} =
        maybe (errAlias a) (pure . (a, )) $ find (cmpNames originalName) decls

    ocuredDecls :: [(Alias, DataDec)] -> [DataDec]
    ocuredDecls = fst . repeatedDecls'

    repeatedDecls :: [(Alias, DataDec)] -> [DataDec]
    repeatedDecls = snd . repeatedDecls'

    repeatedDecls' :: [(Alias, DataDec)] -> ([DataDec], [DataDec])
    repeatedDecls' = repeatedBy (\a b -> declToName a == declToName b)
        . fmap snd

    errAlias Alias{..} = Left $ "Alias " <> newName
        <> "doesn't have corresponding data declaration. "
        <> "Missing data declaration: " <> originalName

    errDataDecl xs = Left $ "To many data declarations. Following data "
        <> "declarations are not accompanied by aliases: "
        <> intercalate ", " (fmap declToName xs)

    cmpNames name DataDec{..} =
        name == declHeadName declarationHead

    declToName DataDec{..} = declHeadName declarationHead

    declHeadName (DHApp _ x _) = declHeadName x
    declHeadName (DHead _ name) = nameToString name
    declHeadName (DHInfix _ _ name) = nameToString name
    declHeadName (DHParen _ x) = declHeadName x

    nameToString (Exts.Ident _ name) = name
    nameToString (Exts.Symbol _ name) = name

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
    createNewDeclHead' (DHead l _) ys [] = pure (DHead l newName, ys)
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

createNewConstructors
    :: forall l
    . [TypeVariablePair]
    -> [Constructor]
    -> QualConDecl ()
    -> Either String (QualConDecl ())
createNewConstructors typeVarPairList conMappings oldCon =
    QualConDecl (annotation oldCon)
    <$> modifyTypeVar (typeVars oldCon)
    <*> pure (context oldCon)
    <*> newConDecl (conDecl oldCon)
  where
    modifyTypeVar
        :: Maybe [TyVarBind ()]
        -> Either String (Maybe [TyVarBind ()])
    modifyTypeVar = pure

    annotation :: QualConDecl () -> ()
    annotation (QualConDecl () _ _ _) = ()

    typeVars :: QualConDecl () -> Maybe [TyVarBind ()]
    typeVars (QualConDecl _ tvs _ _) = tvs

    context :: QualConDecl () -> Maybe (Context ())
    context (QualConDecl _ _ cntx _) = cntx

    conDecl :: QualConDecl () -> ConDecl ()
    conDecl (QualConDecl _ _ _ cdcl) = cdcl

    -- TODO:
    -- rmMappedTyVars

    newConDecl :: ConDecl () -> Either String (ConDecl ())
    newConDecl (ConDecl _ name types) = ConDecl ()
        <$> translateName name
        <*> translateTypes types
    newConDecl InfixConDecl{} =
        Left "Infix data constructor is not supported."
    newConDecl (RecDecl _ name records) = RecDecl ()
        <$> translateName name
        <*> processRecords records
      where
        processRecords = fmap mconcat . mapM (\v -> leftMaybeToError
            $ runReaderT (processRecord v) typeVarPairList)
        processRecord (FieldDecl l2 names t) =
            FieldDecl l2 names <$> translateType t

    translateName :: Exts.Name () -> Either String (Exts.Name ())
    translateName (Exts.Ident _ name) =
        maybeToEither (nameError name) . fmap (Exts.Ident ())
        $ lookup name conMappings
    translateName (Exts.Symbol _ name) =
        maybeToEither (nameError name) . fmap (Exts.Symbol ())
        $ lookup name conMappings

    nameError name = "Can't find constructor \"" <> name <> "\" in constructor "
        <> "mapping: " <> show conMappings

--    translateRecords =

    translateTypes :: [Type ()] -> Either String [Type ()]
    translateTypes types = mconcat
        <$> mapM (\v -> leftMaybeToError
            $ runReaderT (translateType v) typeVarPairList) types

    leftMaybeToError :: Either (Maybe String) a -> Either String [a]
    leftMaybeToError (Right v) = pure [v]
    leftMaybeToError (Left Nothing) = pure []
    leftMaybeToError (Left (Just v)) = Left v

type TransM = ReaderT [TypeVariablePair] (Either (Maybe String))

translateType :: Type l -> TransM (Type l)
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
    classifyLeftSide t1 >>= \case
        Keep -> TyApp l t1 <$> translateType t2
        Rename -> TyApp l <$> renameTyVar t1 <*> translateType t2
        GoDeeper -> TyApp l <$> translateType t1 <*> translateType t2
        Flatten -> translateType t2
        Drop -> lift $ Left Nothing
translateType v@(TyVar _ name) = do
    typeVarPairList <- ask
    case lookup (nameToStr name) typeVarPairList of
        -- TODO: This error message sucks...
        Just DontModify -> pure v
        Just _ -> lift . Left . Just $ "Type variable [" <> prettyPrint name
            <> "] can't be used as final type."
        Nothing -> pure v
translateType v@TyCon{} = pure v
translateType (TyParen l t) = TyParen l <$> translateType t
translateType (TyInfix l t1 name t2) = do
    v <- classifyLeftSide t1
    case v of
        Keep -> TyInfix l t1 <$> pure name <*> translateType t2
        Rename -> TyInfix l
            <$> renameTyVar t1
            <*> pure name
            <*> translateType t2
        GoDeeper -> TyInfix l
            <$> translateType t1
            <*> pure name
            <*> translateType t2
        _ -> lift . Left . Just
            $ "Can't drop infix name: " <> prettyPrint name
translateType (TyKind l t k) = TyKind l <$> translateType t <*> pure k
-- TODO: Promoted types can contain type variables to... This mistake
-- is on multiple places.
-- Let's ignore it for now.
translateType v@TyPromoted{} = pure v
-- TODO: Not sure about this one... Should it behave the same way as TyApp?
translateType (TyEquals l t1 t2) =
    TyEquals l <$> translateType t1 <*> translateType t2
translateType v@TySplice{} = pure v
translateType (TyBang l bt us t) = TyBang l bt us <$> translateType t
translateType v@TyWildCard{} = pure v
translateType v@TyQuasiQuote{} = pure v

classifyLeftSide :: Type l -> TransM Classification
classifyLeftSide TyForall{} = pure Keep
classifyLeftSide TyFun{} = pure GoDeeper
classifyLeftSide TyTuple{} = pure GoDeeper
classifyLeftSide TyUnboxedSum{} = pure GoDeeper
classifyLeftSide (TyList _ t) = classifyLeftSide t
classifyLeftSide (TyParArray _ t) = classifyLeftSide t
classifyLeftSide TyApp{} = pure GoDeeper
classifyLeftSide (TyVar _ name) = do
    typeVarPairList <- ask
    pure $ case lookup (nameToStr name) typeVarPairList of
        Just DontModify -> Keep
        Just Proxy -> Drop
        Just Identity -> Flatten
        Just Maybe -> Rename
        _ -> Keep
classifyLeftSide TyCon{} = pure Keep
classifyLeftSide (TyParen _ t) = classifyLeftSide t
classifyLeftSide TyInfix{} = pure GoDeeper
classifyLeftSide (TyKind _ t _) = classifyLeftSide t
classifyLeftSide TyPromoted{} = pure Keep
classifyLeftSide TyEquals{} = pure GoDeeper
classifyLeftSide TySplice{} = pure Keep
classifyLeftSide (TyBang _ _ _ t) = classifyLeftSide t
classifyLeftSide TyWildCard{} = pure Keep
classifyLeftSide TyQuasiQuote{} = pure Keep

renameTyVar :: forall l. Type l -> TransM (Type l)
renameTyVar (TyList l t) = TyList l <$> renameTyVar t
renameTyVar (TyParArray l t) = TyParArray l <$> renameTyVar t
renameTyVar v@(TyVar l name) = do
    typeVarPairList <- ask
    maybe err toNewVaule $ lookup (nameToStr name) typeVarPairList
  where
    toNewVaule :: TypeVariable -> TransM (Type l)
    toNewVaule Maybe = pure $ TyCon l (UnQual l (Exts.Ident l "Maybe"))
    toNewVaule DontModify = pure v
    toNewVaule x = lift . Left . Just
        $ "Internal error `renameTyVar`. Unsupported TypeVariable constructor: "
        <> show x

    err = lift . Left . Just
        $ "Internal error `renameTyVar`. Type variable is not in type " <>
        "variable list: " <> prettyPrint name
renameTyVar (TyParen l t) = TyParen l <$> renameTyVar t
renameTyVar (TyKind l t k) = (\v -> TyKind l v k) <$> renameTyVar t
renameTyVar (TyBang l bt mpn t) = TyBang l bt mpn <$> renameTyVar t
renameTyVar v = lift . Left . Just
    $ "Internal error renameTyVar: Unsupported type constructor: "
    <> prettyPrint v

data Classification = Keep | Rename | Drop | GoDeeper | Flatten

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = pure a
maybeToEither e Nothing = Left e

eitherToQ :: Either String a -> Q a
eitherToQ = either fail pure

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
