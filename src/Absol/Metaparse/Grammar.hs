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
type Terminal = Text

type RepeatCountSymbol = Terminal
type ExceptSymbol = Terminal
type DisjunctionSymbol = Terminal
type DefiningSymbol = Terminal
type RuleTerminationSymbol = Terminal

type OptionalStartSymbol = Terminal
type OptionalEndSymbol = Terminal
type GroupStartSymbol = Terminal
type GroupEndSymbol = Terminal
type RepeatStartSymbol = Terminal
type RepeatEndSymbol = Terminal

type SpecialSequenceStartSymbol = Terminal
type SpecialSequenceEndSymbol = Terminal

type SemanticBehavesAs = Terminal
type EvaluatesTo = Terminal
type WhereSymbol = Terminal
type SemanticEnd = Terminal
type SemanticAssign = Terminal

type SemanticEnvironmentSymbol = Terminal
type SemanticEnvironmentInputSymbol = Terminal
type EnvironmentAccessSymbol = Terminal
type EnvironmentDefinesSymbol = Terminal
type SemanticListDelimiter = Terminal
type SemanticDisjunction = Terminal

type SemanticBlockStart = Terminal
type SemanticBlockEnd = Terminal
type RestrictionBlockStart = Terminal
type RestrictionBlockEnd = Terminal

type SyntaxAccessStartSymbol = Terminal
type SyntaxAccessEndSymbol = Terminal 

type SpecialSyntaxStart = Terminal
type SpecialSyntaxEnd = Terminal

type Keyword = Text

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
    | LanguageDefblock MetaspecLanguage
    deriving (Show)

newtype UsingList = UsingList [MetaspecFeature] deriving (Show)

newtype SemanticEvaluationList = 
    SemanticEvaluationList [SemanticEvaluation]
    deriving (Show)

type MetaspecFeature = Text

data MetaspecLanguage = MetaspecLanguage deriving (Show)





-- TEMP
newtype SemanticEvaluation = SemanticEvaluation Text deriving (Show)
