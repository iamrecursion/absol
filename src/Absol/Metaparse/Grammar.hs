-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse.Grammar
-- Description : A Haskell representation of the Metaspec Grammar.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains data types that represent the structure of the Metaspec
-- language grammar at the language level.
--
-------------------------------------------------------------------------------
module Absol.Metaparse.Grammar where

-- import           Control.Monad         (void)
import Data.Text
-- import           Text.Megaparsec
-- import           Text.Megaparsec.Char
-- import           Text.Megaparsec.Expr
-- import qualified Text.Megaparsec.Lexer as L
-- import           Text.Megaparsec.Perm
-- import           Text.Megaparsec.Text  (Parser)

-- Basic Terminal Symbol Types
type MetaspecTerminal = Text

type RepeatCountSymbol = MetaspecTerminal
type ExceptSymbol = MetaspecTerminal
type DisjunctionSymbol = MetaspecTerminal
type DefiningSymbol = MetaspecTerminal
type RuleTerminationSymbol = MetaspecTerminal

type OptionalStartSymbol = MetaspecTerminal
type OptionalEndSymbol = MetaspecTerminal
type GroupStartSymbol = MetaspecTerminal
type GroupEndSymbol = MetaspecTerminal
type RepeatStartSymbol = MetaspecTerminal
type RepeatEndSymbol = MetaspecTerminal

type SpecialSequenceStartSymbol = MetaspecTerminal
type SpecialSequenceEndSymbol = MetaspecTerminal

type StartSymbolStart = MetaspecTerminal
type StartSymbolEnd = MetaspecTerminal
type NonTerminalStart = MetaspecTerminal
type NonTerminalEnd = MetaspecTerminal

type SemanticBehavesAs = MetaspecTerminal
type EvaluatesTo = MetaspecTerminal
type WhereSymbol = MetaspecTerminal
type SemanticEnd = MetaspecTerminal
type SemanticAssign = MetaspecTerminal

type SemanticEnvironmentSymbol = MetaspecTerminal
type SemanticEnvironmentInputSymbol = MetaspecTerminal
type EnvironmentAccessSymbol = MetaspecTerminal
type EnvironmentDefinesSymbol = MetaspecTerminal
type SemanticListDelimiter = MetaspecTerminal
type SemanticDisjunction = MetaspecTerminal

type SemanticBlockStart = MetaspecTerminal
type SemanticBlockEnd = MetaspecTerminal
type RestrictionBlockStart = MetaspecTerminal
type RestrictionBlockEnd = MetaspecTerminal

type SyntaxAccessStartSymbol = MetaspecTerminal
type SyntaxAccessEndSymbol = MetaspecTerminal 

type SpecialSyntaxStart = MetaspecTerminal
type SpecialSyntaxEnd = MetaspecTerminal

type LiteralQuote = MetaspecTerminal

type Keyword = Text
type Identifier = Text

-- Keyword Lists
metaspecFeatureList :: [Text]
metaspecFeatureList = 
    [
        "funcall",
        "integer",
        "floating-point",
        "array",
        "text",
        "list",
        "matrix",
        "associative-array",
        "map",
        "reduce",
        "state",
        "maybe",
        "random"
    ]

semanticTypeList :: [Text]
semanticTypeList =
    [
        "uinteger",
        "integer",
        "int32",
        "int64",
        "uint32",
        "uint64",
        "num",
        "text",
        "matrix",
        "array",
        "list",
        "map",
        "any",
        "none",
        "maybe"
    ]

semanticSpecialSyntaxList :: [Text]
semanticSpecialSyntaxList =
    [
        "funcall",
        "array",
        "store",
        "retrieve",
        "map",
        "reduce",
        "apply", 
        "rand"
    ]

-- Defines the Grammar 

-- | Defines the start symbol for the metaspec grammar.
newtype Metaspec = Metaspec [MetaspecDef] deriving (Show)

data MetaspecDef = 
    MetaspecDef MetaspecDefblock RuleTerminationSymbol deriving (Show)

data MetaspecDefblock
    = NameDefblock Keyword WhereSymbol Text
    | VersionDefblock Keyword WhereSymbol Text
    | UsingDefblock 
        Keyword 
        WhereSymbol 
        SemanticBlockStart 
        UsingList 
        SemanticBlockEnd
    | TruthsDefblock
        Keyword 
        WhereSymbol 
        SemanticBlockStart 
        SemanticEvaluationList
        SemanticBlockEnd
    | LanguageDefblock 
        Keyword
        WhereSymbol
        SemanticBlockStart
        LanguageDefinition
        SemanticBlockEnd
    deriving (Show)

newtype UsingList = UsingList [MetaspecFeature] deriving (Show)

newtype SemanticEvaluationList = 
    SemanticEvaluationList [SemanticEvaluation]
    deriving (Show)

type MetaspecFeature = Text

data LanguageDefinition = LanguageDefinition
    [LanguageRule]
    StartRule
    [LanguageRule]
    deriving (Show)

data NonTerminal = NonTerminal
    NonTerminalStart
    Identifier
    NonTerminalEnd
    deriving (Show)

data Terminal = Terminal
    LiteralQuote
    Identifier
    LiteralQuote
    deriving (Show)

data StartSymbol = StartSymbol
    StartSymbolStart
    Identifier
    StartSymbolEnd
    deriving (Show)

data StartRule = StartRule
    StartSymbol
    DefiningSymbol
    LanguageRuleBody
    deriving (Show)

data LanguageRule = LanguageRule
    NonTerminal
    DefiningSymbol
    LanguageRuleBody
    deriving (Show)

data LanguageRuleBody = LanguageRuleBody
    deriving (Show)

-- TEMP
newtype SemanticEvaluation = SemanticEvaluation Text deriving (Show)
