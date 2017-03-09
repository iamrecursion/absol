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

import           Data.Text

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

type Keyword = String
type Identifier = String

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

newtype Metaspec = Metaspec [MetaspecDefblock] deriving (Show)

data MetaspecDefblock
    = NameDefblock String
    | VersionDefblock String
    | UsingDefblock [MetaspecFeature]
    | TruthsDefblock SemanticTruthsList
    | LanguageDefblock StartRule [LanguageRule]
    deriving (Show)

type MetaspecFeature = String

data StartRule = StartRule
    StartSymbol
    LanguageRuleBody
    deriving (Show)

newtype StartSymbol = StartSymbol Identifier deriving (Show)

data LanguageRule = LanguageRule
    NonTerminal
    LanguageRuleBody
    deriving (Show)

newtype LanguageRuleBody = LanguageRuleBody
    SyntaxExpression
    deriving (Show)

newtype SyntaxExpression = SyntaxExpression
    [SyntaxAlternative]
    deriving (Show)

data SyntaxAlternative = SyntaxAlternative
    [SyntaxTerm]
    (Maybe LanguageRuleSemantics)
    deriving (Show)

data SyntaxTerm = SyntaxTerm
    SyntaxFactor
    (Maybe SyntaxException)
    deriving (Show)

newtype SyntaxException = SyntaxException SyntaxFactor deriving (Show)

data SyntaxFactor = SyntaxFactor
    (Maybe RepeatSyntax)
    SyntaxPrimary
    deriving (Show)

newtype RepeatSyntax = RepeatSyntax Integer deriving (Show)

data SyntaxPrimary
    = SyntaxOptional SyntaxExpression
    | SyntaxRepeated SyntaxExpression
    | SyntaxGrouped SyntaxExpression
    | SyntaxSpecial String
    | TerminalProxy Terminal
    | NonTerminalProxy NonTerminal
    | SyntaxEmpty
    deriving (Show)

newtype Terminal = Terminal Identifier deriving (Show)

newtype NonTerminal = NonTerminal Identifier deriving (Show)

newtype LanguageRuleSemantics = LanguageRuleSemantics
    [SemanticRule]
    deriving (Show)

newtype SemanticType = SemanticType String deriving (Show)

data SemanticRule
    = EnvironmentInputRule SemanticType SyntaxAccessBlock SyntaxAccessList
    | EnvironmentAccessRuleProxy EnvironmentAccessRule
    | SpecialSyntaxRuleProxy SpecialSyntaxRule
    | SemanticEvaluationRule
        SemanticType
        SemanticIdentifier
        SemanticOperationList
        SemanticRestrictionList
        SemanticEvaluationList
    deriving (Show)

type SemanticIdentifier = Identifier

newtype SemanticSpecialSyntax = SemanticSpecialSyntax String deriving (Show)

data SpecialSyntaxRule = SpecialSyntaxRule
    (Maybe SemanticType)
    SemanticSpecialSyntax
    [AccessBlockOrRule]
    deriving (Show)

data EnvironmentAccessRule = EnvironmentAccessRule
    (Maybe SemanticType)
    [SyntaxAccessBlock]
    deriving (Show)

type AccessBlockOr a = Either SyntaxAccessBlock a

type AccessBlockOrRule = AccessBlockOr EnvironmentAccessRule

type AccessBlockOrSpecial = AccessBlockOr SpecialSyntaxRule

data SyntaxAccessBlock = SyntaxAccessBlock
    NonTerminal
    SyntaxAccessor
    deriving (Show)

newtype SyntaxAccessor = SyntaxAccessor Integer deriving (Show)

type SyntaxAccessList = [SyntaxAccessBlock]

type SemanticEvaluationList = [SemanticEvaluation]

type SemanticTruthsList = [SemanticTruth]

data SemanticEvaluation = SemanticEvaluation
    SemanticType
    Identifier
    AccessBlockOrSpecial
    deriving (Show)

data SemanticTruth = SemanticTruth
    SemanticType
    Identifier
    NonTerminal
    deriving (Show)

newtype SemanticOperationList = SemanticOperationList
    [SemanticOperationAssignment] -- Sep by ','
    deriving (Show)

data SemanticOperationAssignment = SemanticOperationAssignment
    Identifier
    SemanticOperation
    deriving (Show)

-- TODO update real grammar to reflect this
data SemanticOperation
    = Variable Identifier
    | Constant SemanticValue
    | Parentheses SemanticOperation
    | PrefixExpr PrefixSemanticUnaryOperator SemanticOperation
    | PostfixExpr PostfixSemanticUnaryOperator SemanticOperation
    | InfixExpr SemanticBinaryOperator SemanticOperation SemanticOperation
    deriving (Show)

data PrefixSemanticUnaryOperator
    = Not
    | Negate
    | PreIncrement
    | PreDecrement
    deriving (Show)

data PostfixSemanticUnaryOperator
    = PostIncrement
    | PostDecrement
    deriving (Show)

data SemanticBinaryOperator
    = Plus
    | Minus
    | Times
    | Divide
    | Exponent
    | BitOr
    | Or
    | And
    | BitAnd
    | EqualTo
    | NotEqualTo
    | LessThan
    | GreaterThan
    | LEQ
    | GEQ
    deriving (Show)

newtype SemanticRestrictionList = SemanticRestrictionList
    [SemanticRestriction]
    deriving (Show)

data SemanticRestriction
    = SemVariable Identifier
    | SemConstant SemanticValue
    | SemInfixExpr
        SemanticRestrictionOperator
        SemanticRestriction
        SemanticRestriction
    deriving (Show)

data SemanticValue
    = SemanticText String
    | SemanticNumber Integer
    | SemanticBoolean Bool
    deriving (Show)

data SemanticRestrictionOperator
    = SemEquals
    | SemNEquals
    | SemLT
    | SemGT
    | SemLEQ
    | SemGEQ
    deriving (Show)
