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
type SemanticType = Text

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

newtype UsingList = UsingList [MetaspecFeature] deriving (Show) -- sep by ','

type MetaspecFeature = Text

data LanguageDefinition = LanguageDefinition
    [LanguageRule]
    StartRule
    [LanguageRule]
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
    SyntaxExpression
    RuleTerminationSymbol
    deriving (Show)

newtype SyntaxExpression = SyntaxExpression
    [SyntaxAlternative] -- Separated by '|'
    deriving (Show)

data SyntaxAlternative = SyntaxAlternative 
    [SyntaxTerm] 
    Maybe LanguageRuleSemantics
    deriving (Show)

data SyntaxTerm = SyntaxTerm
    SyntaxFactor 
    Maybe SyntaxException
    deriving (Show)

data SyntaxException = SyntaxException
    ExceptSymbol 
    SyntaxFactor
    deriving (Show)

data SyntaxFactor = SyntaxFactor
    Maybe RepeatSyntax
    SyntaxPrimary
    deriving (Show)

data RepeatSyntax = RepeatSyntax
    Integer
    RepeatCountSymbol
    deriving (Show)

data SyntaxPrimary
    = SyntaxOptional OptionalStartSymbol SyntaxExpression OptionalEndSymbol
    | SyntaxRepeated RepeatStartSymbol SyntaxExpression RepeatEndSymbol
    | SyntaxGrouped GroupStartSymbol SyntaxExpression GroupEndSymbol
    | SyntaxSpecial 
        SpecialSequenceStartSymbol 
        SyntaxExpression
        SpecialSequenceEndSymbol
    | SyntaxEmpty
    | Terminal LiteralQuote Identifier LiteralQuote
    | NonTerminal NonTerminalStart Identifier NonTerminalEnd
    deriving (Show)

data LanguageRuleSemantics = LanguageRuleSemantics
    SemanticBehavesAs
    SemanticBlockStart
    [SemanticRule] -- sep by '|'
    SemanticBlockEnd
    deriving (Show)

data SemanticRule
    = EnvironmentInputRule
        SemanticType
        SemanticEnvironmentSymbol
        SemanticEnvironmentInputSymbol
        SyntaxAccessBlock
        EnvironmentDefinesSymbol
        SyntaxAccessList
    | EnvironmentAccessRule
        SemanticEnvironmentSymbol
        EnvironmentAccessSymbol
        [SyntaxAccessBlock]
    | SpecialSyntaxRule
        SemanticSpecialSyntax
        SpecialSyntaxStart
        Maybe [AccessBlockOrRule]
        SpecialSyntaxEnd
    | SemanticEvaluationRule
        SemanticType
        SemanticIdentifier
        WhereSymbol
        SemanticOperationList
        SemanticRestrictionList
        WhereSymbol
        SemanticEvaluationList
    deriving (Show)

type SemanticIdentifier = Identifier
type SemanticSpecialSyntax = Text

newtype AccessBlockOr a = AccessBlockOr
    Either SyntaxAccessBlock a
    deriving (Show)

newtype AccessBlockOrRule = AccessBlockOrRule 
    AccessBlockOr EnvironmentAccessRule
    deriving (Show)

newtype AccessBlockOrSpecial = AccessBlockOrSpecial
    AccessBlockOr SpecialSyntaxRule
    deriving (Show)

data SyntaxAccessBlock = SyntaxAccessBlock
    NonTerminal 
    SyntaxAccessor
    deriving (Show)

data SyntaxAccessor = SyntaxAccessor
    SyntaxAccessStartSymbol
    Integer -- unsigned
    SyntaxAccessEndSymbol
    deriving (Show)

-- Separated by ','
newtype SyntaxAccessList = SyntaxAccessList [SyntaxAccessBlock] deriving (Show)

-- Separated by ','
data SemanticEvaluationList = SemanticEvaluationList [SemanticEvaluation]
    deriving (Show)

data SemanticEvaluation = SemanticEvaluation
    SemanticBlockStart
    SemanticType
    Identifier
    EvaluatesTo
    AccessBlockOrSpecial
    SemanticBlockEnd
    deriving (Show)

data SemanticOperationList = SemanticOperationList
    SemanticBlockStart
    [SemanticOperationAssignment] -- Sep by ','
    SemanticBlockEnd
    deriving (Show)

data SemanticOperationAssignment = SemanticOperationAssignment
    Identifier
    SemanticAssign
    SemanticOperation
    deriving (Show)

data SemanticOperation
    = PrefixUnaryOpExpression 
        PrefixSemanticUnaryOperator 
        Either Identifier PrefixUnaryOpExpression
    | PostfixUnaryOpExpression 
        Either Identifier PostfixUnaryOpExpression
        PostfixSemanticUnaryOperator
    | BinaryOpExpression 
        Either Identifier BinaryOpExpression
        SemanticBinaryOperator
        Either Identifier BinaryOpExpression
    deriving (Show)

data SemanticRestrictionList = SemanticRestrictionList
    RestrictionBlockStart
    [SemanticRestriction] -- Sep by ','
    RestrictionBlockEnd
    deriving (Show)

data SemanticRestriction = SemanticRestriction
    Identifier
    SemanticRestrictionCheckOperator
    Either Identifier SemanticRestrictionValue
    deriving (Show)

data SemanticRestrictionValue 
    = SemanticText Text
    | SemanticNumber Integer
    | SemanticBoolean Bool

data SemanticRestrictionCheckOperator 
    = SemEquals
    | SemNEquals
    | SemLT
    | SemGT
    | SemLEQ
    | SemGEQ
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
    | BitOR
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
