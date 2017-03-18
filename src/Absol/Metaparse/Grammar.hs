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

-- Basic Terminal Symbol Types
type MetaspecTerminal = String

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
type SemanticAnd = MetaspecTerminal
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

type OpenParenthesis = MetaspecTerminal
type CloseParenthesis = MetaspecTerminal

-- Identifier Types
newtype NonTerminalIdentifier = NonTerminalIdentifier String 
    deriving (Show, Eq, Ord)
newtype TerminalString = TerminalString String deriving (Show, Eq, Ord)
newtype SemanticIdentifier = SemanticIdentifier String deriving (Show, Eq, Ord)
newtype SemanticType = SemanticType String deriving (Show, Eq, Ord)

-- Defines the Grammar
newtype Metaspec = Metaspec [MetaspecDefblock] deriving (Show)

data MetaspecDefblock
    = NameDefblock String
    | VersionDefblock String
    | UsingDefblock [MetaspecFeature]
    | TruthsDefblock SemanticTruthsList
    | LanguageDefblock StartRule [LanguageRule]
    deriving (Show)

newtype MetaspecFeature = MetaspecFeature String deriving (Show)

data StartRule = StartRule
    StartSymbol
    LanguageRuleBody
    deriving (Show)

newtype StartSymbol = StartSymbol NonTerminalIdentifier deriving (Show)

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

newtype Terminal = Terminal TerminalString deriving (Show)

newtype NonTerminal = NonTerminal NonTerminalIdentifier deriving (Show)

newtype LanguageRuleSemantics = LanguageRuleSemantics
    [SemanticRule]
    deriving (Show)

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
    SemanticIdentifier
    AccessBlockOrSpecial
    deriving (Show)

data SemanticTruth = SemanticTruth
    SemanticType
    SemanticIdentifier
    NonTerminal
    deriving (Show)

newtype SemanticOperationList = SemanticOperationList
    [SemanticOperationAssignment] -- Sep by ','
    deriving (Show)

data SemanticOperationAssignment = SemanticOperationAssignment
    SemanticIdentifier
    SemanticOperation
    deriving (Show)

-- TODO update real grammar to reflect this
data SemanticOperation
    = Variable SemanticIdentifier
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
    = SemVariable SemanticIdentifier
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
