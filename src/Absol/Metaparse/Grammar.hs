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

-- Semantic Types
data SemanticType 
    = AnyType
    | NoneType
    | BoolType
    | NaturalType
    | IntegerType
    | Int32Type
    | UInt32Type
    | Int64Type
    | UInt64Type
    | FloatType
    | DoubleType
    | IntegralType
    | FloatingType
    | NumberType
    | StringType
    | ListType
    | MatrixType
    deriving (Show, Eq, Ord)

-- Identifier Types
newtype NonTerminalIdentifier = NonTerminalIdentifier String 
    deriving (Show, Eq, Ord)
newtype TerminalString = TerminalString String deriving (Show, Eq, Ord)
newtype SemanticIdentifier = SemanticIdentifier String deriving (Show, Eq, Ord)

-- Defines the Grammar
newtype Metaspec = Metaspec [MetaspecDefblock] deriving (Show, Eq)

data MetaspecDefblock
    = NameDefblock String
    | VersionDefblock String
    | UsingDefblock [MetaspecFeature]
    | TruthsDefblock SemanticTruthsList
    | LanguageDefblock StartRule [LanguageRule]
    deriving (Show, Eq)

data MetaspecFeature 
    = FeatureBase
    | FeatureNumber
    | FeatureString
    | FeatureList
    | FeatureMatrix
    | FeatureTraverse
    | FeatureFuncall
    deriving (Show, Eq)

data StartRule = StartRule
    StartSymbol
    LanguageRuleBody
    deriving (Show, Eq)

newtype StartSymbol = StartSymbol NonTerminalIdentifier deriving (Show, Eq)

data LanguageRule = LanguageRule
    NonTerminal
    LanguageRuleBody
    deriving (Show, Eq)

newtype LanguageRuleBody = LanguageRuleBody
    SyntaxExpression
    deriving (Show, Eq)

newtype SyntaxExpression = SyntaxExpression
    [SyntaxAlternative]
    deriving (Show, Eq)

data SyntaxAlternative = SyntaxAlternative
    [SyntaxTerm]
    (Maybe LanguageRuleSemantics)
    deriving (Show, Eq)

data SyntaxTerm = SyntaxTerm
    SyntaxFactor
    (Maybe SyntaxException)
    deriving (Show, Eq)

newtype SyntaxException = SyntaxException SyntaxFactor deriving (Show, Eq)

data SyntaxFactor = SyntaxFactor
    (Maybe RepeatSyntax)
    SyntaxPrimary
    deriving (Show, Eq)

newtype RepeatSyntax = RepeatSyntax Integer deriving (Show, Eq)

data SyntaxPrimary
    = SyntaxOptional SyntaxExpression
    | SyntaxRepeated SyntaxExpression
    | SyntaxGrouped SyntaxExpression
    | SyntaxSpecial String
    | TerminalProxy Terminal
    | NonTerminalProxy NonTerminal
    deriving (Show, Eq)

newtype Terminal = Terminal TerminalString deriving (Show, Eq)

newtype NonTerminal = NonTerminal NonTerminalIdentifier deriving (Eq, Ord)

instance Show NonTerminal where
    show (NonTerminal (NonTerminalIdentifier x)) = "<" ++ x ++ ">"

newtype LanguageRuleSemantics = LanguageRuleSemantics
    [SemanticRule]
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data SemanticSpecialSyntax
    = SpecialSyntaxMap
    | SpecialSyntaxFold
    | SpecialSyntaxFilter
    | SpecialSyntaxDefproc
    | SpecialSyntaxDeffun
    | SpecialSyntaxCallproc
    | SpecialSyntaxCallfun
    deriving (Show, Eq)

data SpecialSyntaxRule = SpecialSyntaxRule
    SemanticType
    SemanticSpecialSyntax
    [AccessBlockOrRule]
    deriving (Show, Eq)

data EnvironmentAccessRule = EnvironmentAccessRule
    (Maybe SemanticType)
    [SyntaxAccessBlock]
    deriving (Show, Eq)

type AccessBlockOr a = Either SyntaxAccessBlock a

type AccessBlockOrRule = AccessBlockOr EnvironmentAccessRule

type AccessBlockOrSpecial = AccessBlockOr SpecialSyntaxRule

data SyntaxAccessBlock = SyntaxAccessBlock
    NonTerminal
    SyntaxAccessor
    deriving (Show, Eq)

newtype SyntaxAccessor = SyntaxAccessor Integer deriving (Show, Eq)

type SyntaxAccessList = [SyntaxAccessBlock]

type SemanticEvaluationList = [SemanticEvaluation]

type SemanticTruthsList = [SemanticTruth]

data SemanticEvaluation = SemanticEvaluation
    SemanticType
    SemanticIdentifier
    AccessBlockOrSpecial
    deriving (Show, Eq)

data SemanticTruth = SemanticTruth
    SemanticType
    SemanticIdentifier
    NonTerminal
    deriving (Show, Eq)

newtype SemanticOperationList = SemanticOperationList
    [SemanticOperationAssignment] -- Sep by ','
    deriving (Show, Eq)

data SemanticOperationAssignment = SemanticOperationAssignment
    SemanticIdentifier
    SemanticOperation
    deriving (Show, Eq)

-- TODO update real grammar to reflect this
data SemanticOperation
    = Variable SemanticIdentifier
    | VariableAccess SemanticIdentifier [Integer]
    | Constant SemanticValue
    | Parentheses SemanticOperation
    | PrefixExpr PrefixSemanticUnaryOperator SemanticOperation
    | PostfixExpr PostfixSemanticUnaryOperator SemanticOperation
    | InfixExpr SemanticBinaryOperator SemanticOperation SemanticOperation
    deriving (Show, Eq)

data PrefixSemanticUnaryOperator
    = Not
    | Negate
    | PreIncrement
    | PreDecrement
    deriving (Show, Eq)

data PostfixSemanticUnaryOperator
    = PostIncrement
    | PostDecrement
    deriving (Show, Eq)

data SemanticBinaryOperator
    = Plus
    | Minus
    | Cons
    | Times
    | Divide
    | Modulo
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
    deriving (Show, Eq)

newtype SemanticRestrictionList = SemanticRestrictionList
    [SemanticRestriction]
    deriving (Show, Eq)

data SemanticRestriction
    = SemVariable SemanticIdentifier
    | SemConstant SemanticValue
    | SemInfixExpr
        SemanticRestrictionOperator
        SemanticRestriction
        SemanticRestriction
    deriving (Show, Eq)

data SemanticValue
    = SemanticText String
    | SemanticNumber Integer
    | SemanticBoolean Bool
    | SemanticListLiteral [String]
    | SemanticMatrixLiteral [[String]]
    deriving (Show, Eq)

data SemanticRestrictionOperator
    = SemEquals
    | SemNEquals
    | SemLT
    | SemGT
    | SemLEQ
    | SemGEQ
    deriving (Show, Eq)
