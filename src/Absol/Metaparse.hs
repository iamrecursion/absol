-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse
-- Description : Parser combinators for parsing Metaspec.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains the combinator parsers for parsing metaspec files. It
-- provides robust error reporting functionality and efficient parser
-- construction.
--
-------------------------------------------------------------------------------
module Absol.Metaparse
    (
        parseMetaspecFile,
        parseMetaspec
    ) where

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Absol.Metaparse.Utilities
import           Absol.Metaparse.Parser
import           Control.Monad (void)
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Expr

-------------------------------------------------------------------------------

-- TODO Stateful parsing:
--      + Add state to the parser so it can track the using types.
--      + Track the defined non-terminals.
--      + Ensure that no non-terminal is defined more than once. 
--      + Error if a type is used without being imported or doesn't exist.
--      + Check keywords against the allowed list from imports.
--      + Check if keywords exist. 
--      + For non-imported types suggest the import.

-- TODO Special Syntax and Types (Using):
--      + Define the list of using keywords.
--      + For each, define the:
--          > Types they import
--          > Keywords they import 
--          > Operations they allow
--          > Semantics of these operations (sketch termination properties).
--      + Modules for the special syntax Metaspec.Special.x (all exposed via
--      Metaspec.Special)
--      + Make each special syntax a part of the grammar (properly).

-- TODO Rethink Semantic Restrictions:
--      + Nest environment accesses and stores in more places in the semantics.
--      + Needs to be more sophisticated.
--      + Should allow literals of any type in the retrictions.
--      + Should allow literals of any type in the semantic combination blocks.

-- TODO General:
--      + Update EBNF grammar to reflect these changes.
--      + Refactor 'Either' out of the grammar where possible.

-------------------------------------------------------------------------------

-- | Parses a metaspec file.
-- 
-- The file is taken as input and the corresponding parse-tree or error state is
-- returned. 
parseMetaspecFile :: Text -> IO ()
parseMetaspecFile = parseTest parseMetaspec

-- | Parses the top-level metaspec language definition.
parseMetaspec :: Parser Metaspec
parseMetaspec = between spaceConsumer eof metaspec

-- | Parses the top level metaspec definition blocks. 
-- 
-- The top-level definitions are parsed in the specified order, ensuring that
-- contextual information is provided in order. 
metaspec :: Parser Metaspec
metaspec = Metaspec <$> traverse (<* ruleTerminationSymbol) blocks
    where
        blocks =
            [
                nameDefblock, 
                versionDefblock, 
                usingDefblock, 
                truthsDefblock, 
                languageDefblock
            ]

-- | Parses the language name definition block.
-- 
-- It strips whitespace from the start and end of the provided name in the file.
nameDefblock :: Parser MetaspecDefblock
nameDefblock = do
    keywordWhere "name"
    name <- some nonSemicolon
    return (NameDefblock $ trimString name)

-- | Parses the language version definition block.
-- 
-- It strips whitespace from the start and end of the provided version string. 
versionDefblock :: Parser MetaspecDefblock
versionDefblock = do
    keywordWhere "version"
    version <- some nonSemicolon
    return (VersionDefblock $ trimString version)

-- | Parses the using definition block for language features.
usingDefblock :: Parser MetaspecDefblock
usingDefblock = do
    keywordWhere "using"
    items <- semanticBlock $ metaspecFeature `sepBy` multilineListSep
    return (UsingDefblock items)

-- | Parses the list of language features.
-- 
-- The list is delimited by ','.
metaspecFeature :: Parser MetaspecFeature
metaspecFeature = some (alphaNumChar <|> oneOf allowedSeps)
    where
        allowedSeps = "_-" :: String

-- | Parses the list of language truths.
-- 
-- These are used as termination cases by the termination proof engine for the
-- language. The list of truths is delimited by ','.
truthsDefblock :: Parser MetaspecDefblock
truthsDefblock = do
    keywordWhere "truths"
    items <- semanticBlock semanticTruthsList
    return (TruthsDefblock items)

-- | Parses the language definition block.
languageDefblock :: Parser MetaspecDefblock
languageDefblock = do
    keywordWhere "language"
    semanticBlock languageDefinition

-- | Parses the productions that define the language in metaspec.
-- 
-- The language start rule must be defined once in the file, and must be defined
-- before any other language productions in this block.
languageDefinition :: Parser MetaspecDefblock
languageDefinition = do
    start <- startRule
    productions <- languageRule `sepBy` spaceConsumer
    return (LanguageDefblock start productions)

-- | Parses language productions.
languageRule :: Parser LanguageRule
languageRule = do
    prodName <- nonTerminal
    void definingSymbol
    ruleBody <- languageRuleBody
    return (LanguageRule prodName ruleBody)

-- | Parses the language start rule.
startRule :: Parser StartRule
startRule = do
    startSym <- startSymbol
    void definingSymbol
    ruleBody <- languageRuleBody
    return (StartRule startSym ruleBody)

-- | Parses the language start symbol. 
startSymbol :: Parser StartSymbol
startSymbol = do
    ident <- startSymbolDelim nonTerminalIdentifier
    return (StartSymbol ident)

-- | Parses the body of a production.
-- 
-- The body of a production is the portion after the defining symbol.
languageRuleBody :: Parser LanguageRuleBody
languageRuleBody = do
    syntaxExpr <- syntaxExpression
    void ruleTerminationSymbol
    return (LanguageRuleBody syntaxExpr)

-- | Parses a syntax expression.
-- 
-- Syntax expressions consist of the top-level alternatives for a given
-- production.
syntaxExpression :: Parser SyntaxExpression
syntaxExpression = do
    alternatives <- syntaxAlternative `sepBy1` multilineAlternative
    return (SyntaxExpression alternatives)

-- | Parses each top-level alternative.
syntaxAlternative :: Parser SyntaxAlternative
syntaxAlternative = do
    terms <- syntaxTerm `sepBy1` space
    semantics <- option Nothing languageRuleSemantics
    return (SyntaxAlternative terms semantics)

-- | Parses a syntactic term.
-- 
-- Syntactic terms may contain exception syntax. An exception is another syntax
-- definition that is subtracted from the set of allowed syntax defined by the
-- first production.
syntaxTerm :: Parser SyntaxTerm
syntaxTerm = do
    factor <- syntaxFactor
    exception <- option Nothing $ try syntaxException
    return (SyntaxTerm factor exception)

-- | Parses the syntax exception itself.
syntaxException :: Parser (Maybe SyntaxException)
syntaxException = do
    void exceptSymbol
    term <- syntaxFactor
    return (Just $ SyntaxException term)

-- | Parses syntactic factors.
-- 
-- These may optionally define a number of repetitions of the group.
syntaxFactor :: Parser SyntaxFactor
syntaxFactor = do
    repeatChar <- option Nothing repeatSyntax
    prim <- syntaxPrimary
    return (SyntaxFactor repeatChar prim)

-- | Parses the optional repetition syntax. 
repeatSyntax :: Parser (Maybe RepeatSyntax)
repeatSyntax = do
    repeatCount <- naturalNumber
    void repeatCountSymbol
    return (Just $ RepeatSyntax repeatCount)

-- | Parses the syntactic primary expressions.
syntaxPrimary :: Parser SyntaxPrimary
syntaxPrimary = syntaxOptional
    <|> syntaxRepeated
    <|> syntaxGrouped
    <|> syntaxSpecial
    <|> terminalProxy
    <|> nonTerminalProxy

-- | Parses an optional piece of syntax.
syntaxOptional :: Parser SyntaxPrimary
syntaxOptional = SyntaxOptional <$> grammarOptional syntaxExpression

-- | Parses a repeated syntax block.
syntaxRepeated :: Parser SyntaxPrimary
syntaxRepeated = SyntaxRepeated <$> grammarRepeat syntaxExpression

-- | Parses a syntax group.
syntaxGrouped :: Parser SyntaxPrimary
syntaxGrouped = SyntaxGrouped <$> grammarGroup syntaxExpression

-- | Parses a special syntax block.
-- 
-- Special syntax is used for language extensions and is currently not handled
-- by the metacompiler.
syntaxSpecial :: Parser SyntaxPrimary
syntaxSpecial = do
    void specialSequenceStartSymbol
    specialExpr <- parseString
    void specialSequenceEndSymbol
    return (SyntaxSpecial $ trimString specialExpr)

-- | Parses a proxy for a terminal symbol of the language.
terminalProxy :: Parser SyntaxPrimary
terminalProxy = TerminalProxy <$> parseTerminal

-- | Parses a proxy for the non-terminal symbol of the language.
nonTerminalProxy :: Parser SyntaxPrimary
nonTerminalProxy = NonTerminalProxy <$> nonTerminal

-- | Parses a terminal symbol for the language.
parseTerminal :: Parser Terminal
parseTerminal = Terminal <$> terminalString

-- | Parses a non-terminal symbol for the language.
nonTerminal :: Parser NonTerminal
nonTerminal = NonTerminal <$> nonTerminalDelim nonTerminalIdentifier

-- | Parses the optional language rule semantics.
languageRuleSemantics :: Parser (Maybe LanguageRuleSemantics)
languageRuleSemantics = do
    void semanticBehavesAs
    rules <- semanticBlock $ semanticRule `sepBy1` multilineAlternative
    return (Just $ LanguageRuleSemantics rules)

-- | Parses a semantic evaluation rule.
-- 
-- As the rules diverge only after consuming some portion of syntax, this parser
-- utilises the ability for infinite-lookahead backtracking to parse these 
-- productions.
semanticRule :: Parser SemanticRule
semanticRule = try semanticEvaluationRule 
    <|> try environmentInputRule
    <|> try environmentAccessRuleProxy
    <|> specialSyntaxRuleProxy

-- | Parses an environment input rule.
--
-- Such rules deal with storing information (from the syntax) into the language
-- environment.
environmentInputRule :: Parser SemanticRule
environmentInputRule = do
    exprType <- semanticType
    void semanticEnvironmentSymbol
    void semanticEnvironmentInputSymbol
    syntaxBlock <- syntaxAccessBlock
    void environmentDefinesSymbol
    syntaxList <- syntaxAccessList
    return (EnvironmentInputRule exprType syntaxBlock syntaxList)

-- | Parses a special syntax rule.
-- 
-- Such rules utilise one (or more) of the provided special functions to assist
-- in defining the semantics of the production.
specialSyntaxRule :: Parser SpecialSyntaxRule
specialSyntaxRule = do
    specialType <- option Nothing maybeSemanticType
    specialOp <- semanticSpecialSyntax
    semanticBlocks <- specialSyntaxBlock $ 
        accessBlockOrRule `sepBy` multilineListSep
    return (SpecialSyntaxRule specialType specialOp semanticBlocks)

-- | Parses an environment access rule.
-- 
-- These rules are used to retrieve information that has been previously stored
-- in the semantic environment.
environmentAccessRule :: Parser EnvironmentAccessRule
environmentAccessRule = do
    semType <- option Nothing maybeSemanticType
    void semanticEnvironmentSymbol
    void environmentAccessSymbol
    accessBlocks <- syntaxAccessBlock `sepBy` environmentAccessSymbol
    return (EnvironmentAccessRule semType accessBlocks)

-- | Parses a semantic evaluation rule.
-- 
-- These are the rules that are checked directly by the proof mechanism, and
-- they are restricted to being defined in a certain form.
semanticEvaluationRule :: Parser SemanticRule
semanticEvaluationRule = do
    exprType <- semanticType
    semIdentifier <- semanticIdentifier
    void whereSymbol
    semOpList <- semanticOperationList
    semRestrictList <- semanticRestrictionList
    void whereSymbol
    semEvalList <- semanticEvaluationList
    return (SemanticEvaluationRule
        exprType
        semIdentifier
        semOpList
        semRestrictList
        semEvalList
        )

-- | Parses a proxy type for environment access rules.
environmentAccessRuleProxy :: Parser SemanticRule
environmentAccessRuleProxy =
    EnvironmentAccessRuleProxy <$> environmentAccessRule

-- | Parses a proxy type for special syntax rules.
specialSyntaxRuleProxy :: Parser SemanticRule
specialSyntaxRuleProxy = SpecialSyntaxRuleProxy <$> specialSyntaxRule

-- | Parses a syntax access block or environment access rule.
accessBlockOrRule :: Parser AccessBlockOrRule
accessBlockOrRule = eitherP syntaxAccessBlock environmentAccessRule

-- | Parses a syntax access block or special syntax rule.
accessBlockOrSpecial :: Parser AccessBlockOrSpecial
accessBlockOrSpecial = eitherP syntaxAccessBlock specialSyntaxRule

-- | Parses a syntax access blocks.
-- 
-- Such blocks are used within the semantics to refer to portions of syntax that
-- are defined in the grammar production.
syntaxAccessBlock :: Parser SyntaxAccessBlock
syntaxAccessBlock = do
    nt <- nonTerminal
    address <- syntaxAccessor
    return (SyntaxAccessBlock nt address)

-- | Parses the syntax accessor address.
syntaxAccessor :: Parser SyntaxAccessor
syntaxAccessor = SyntaxAccessor <$> syntaxAccess naturalNumber

-- | Parses a list of syntax access blocks.
syntaxAccessList :: Parser SyntaxAccessList
syntaxAccessList = syntaxAccessBlock `sepBy` multilineListSep

-- | Parses a list of semantic truths.
-- 
-- Semantic truths are used to define the termination cases for the language 
-- semantics.
semanticTruthsList :: Parser SemanticTruthsList
semanticTruthsList = semanticTruthBlock `sepBy` multilineListSep

-- | Parses a semantic truth block.
semanticTruthBlock :: Parser SemanticTruth
semanticTruthBlock = semanticBlock semanticTruth

-- | Parses a semantic truth.
-- 
-- Each of these expressions is assumed to terminate by the proof engine.
semanticTruth :: Parser SemanticTruth
semanticTruth = do
    semType <- semanticType
    semId <- semanticIdentifier
    void evaluatesTo
    nt <- nonTerminal
    return (SemanticTruth semType semId nt)

-- | Parses a list of semantic evaluations.
semanticEvaluationList :: Parser SemanticEvaluationList
semanticEvaluationList = semanticEvaluation `sepBy1` multilineListSep

-- | Parses a semantic evaluation.
semanticEvaluation :: Parser SemanticEvaluation
semanticEvaluation = semanticBlock semanticEvaluationBody

-- | Parses the body of a semantic evaluation expression.
-- 
-- These expressions are then composed to define the language rule semantics.
semanticEvaluationBody :: Parser SemanticEvaluation
semanticEvaluationBody = do
    semType <- semanticType
    semID <- semanticIdentifier
    void evaluatesTo
    block <- accessBlockOrSpecial
    return (SemanticEvaluation semType semID block)

-- | Parses a list of semantic operations.
-- 
-- These semantic operations define how the semantic evaluations are combined to
-- define the actual operation of the language.
semanticOperationList :: Parser SemanticOperationList
semanticOperationList = do
    let parseExpr = semanticOperationAssignment `sepBy1` multilineListSep
    operations <- semanticBlock parseExpr
    return (SemanticOperationList operations)

-- | Parses the evaluation operation of the semantics and its binding.
semanticOperationAssignment :: Parser SemanticOperationAssignment
semanticOperationAssignment = do
    semId <- semanticIdentifier
    void semanticAssign
    semOp <- semanticOperation
    return (SemanticOperationAssignment semId semOp)

-- | Parses the allowed semantic combination operations.
semanticOperation :: Parser SemanticOperation
semanticOperation = makeExprParser semanticExpression semanticOperatorTable

-- | Parses the allowed semantic combination expressions.
semanticExpression :: Parser SemanticOperation
semanticExpression = parentheses semanticOperation
    <|> Variable <$> semanticIdentifier
    <|> Constant <$> semanticValue

-- | The operator table for the semantic operations.
-- 
-- Order in the top-level list defines precedence, and order in the sub-lists
-- defines parse order. 
semanticOperatorTable :: [[Operator Parser SemanticOperation]]
semanticOperatorTable =
    [
        [
            Prefix (PrefixExpr Not <$ operator "!"),
            Prefix (PrefixExpr Negate <$ operator "-"),
            Prefix (PrefixExpr PreDecrement <$ operator "--"),
            Prefix (PrefixExpr PreIncrement <$ operator "++")
        ],
        [
            Postfix (PostfixExpr PostDecrement <$ operator "--"),
            Postfix (PostfixExpr PostIncrement <$ operator "++")
        ],
        [
            InfixL (InfixExpr Exponent <$ operator "^")
        ],
        [
            InfixL (InfixExpr Times <$ operator "*"),
            InfixL (InfixExpr Divide <$ operator "/")
        ],
        [
            InfixL (InfixExpr Plus <$ operator "+"),
            InfixL (InfixExpr Minus <$ operator "-")
        ],
        [
            InfixL (InfixExpr BitOr <$ operator "|"),
            InfixL (InfixExpr BitAnd <$ operator "&")
        ],
        [
            InfixL (InfixExpr And <$ operator "&&"),
            InfixL (InfixExpr Or <$ operator "||")
        ],
        [
            InfixN (InfixExpr EqualTo <$ operator "=="),
            InfixN (InfixExpr NotEqualTo <$ operator "!="),
            InfixN (InfixExpr LessThan <$ operator "<"),
            InfixN (InfixExpr GreaterThan <$ operator ">"),
            InfixN (InfixExpr LEQ <$ operator "<="),
            InfixN (InfixExpr GEQ <$ operator ">=")
        ]
    ]

-- | Parses the semantic special syntax expressions.
semanticSpecialSyntax :: Parser SemanticSpecialSyntax
semanticSpecialSyntax = SemanticSpecialSyntax <$> semanticTypeString

-- | Parses a list of semantic restrictions.
-- 
-- These restrictions allow control over which semantic behaviour is performed
-- conditional on the evaluation of the sub-terms.
semanticRestrictionList :: Parser SemanticRestrictionList
semanticRestrictionList = do
    let parseExpr = semanticRestriction `sepBy` multilineListSep
    blocks <- restrictionBlock parseExpr
    return (SemanticRestrictionList blocks)

-- | Parses a semantic restriction.
semanticRestriction :: Parser SemanticRestriction
semanticRestriction =
    makeExprParser semanticRestrictionExpr semanticRestrictionOperator

-- | Parses semantic restriction expressions.
semanticRestrictionExpr :: Parser SemanticRestriction
semanticRestrictionExpr = SemVariable <$> semanticIdentifier
    <|> SemConstant <$> semanticValue

-- | Defines the operator table for the semantic restrictions.
-- 
-- All operators have the same precedence, with parse order defined by the order
-- in the list.
semanticRestrictionOperator :: [[Operator Parser SemanticRestriction]]
semanticRestrictionOperator =
    [
        [
            InfixN (SemInfixExpr SemEquals <$ operator "=="),
            InfixN (SemInfixExpr SemNEquals <$ operator "!="),
            InfixN (SemInfixExpr SemLT <$ operator "<"),
            InfixN (SemInfixExpr SemGT <$ operator ">"),
            InfixN (SemInfixExpr SemLEQ <$ operator "<="),
            InfixN (SemInfixExpr SemGEQ <$ operator ">=")
        ]
    ]

-- TODO make this more sophisticated (should match on constructors of all types)
-- | Parses a semantic value.
-- 
-- These are constants of any allowable type.
semanticValue :: Parser SemanticValue
semanticValue = semanticText
    <|> semanticNumber
    <|> semanticBoolean

-- | Parses a piece of constant semantic text.
semanticText :: Parser SemanticValue
semanticText = do
    let parseExpr = many nonEmptyChar
    textVal <- stringLiteral
    return (SemanticText textVal)

-- | Parses a numerical constant.
semanticNumber :: Parser SemanticValue
semanticNumber = SemanticNumber <$> integer

-- | Parses a boolean constant.
semanticBoolean :: Parser SemanticValue
semanticBoolean = (terminal "true" *> pure (SemanticBoolean True)) 
    <|> (terminal "false" *> pure (SemanticBoolean False))

-- | Parses a semantic type expression.
--
-- This parser will only allow types that are in scope to be parsed, and will
-- raise an error if the type is not in scope or does not exist.
semanticType :: Parser SemanticType
semanticType = SemanticType <$> semanticTypeString <* spaceConsumer

-- | Parses a semantic type that may not exist.
-- 
-- There are certain expressions where the inclusion of the type is optional.
-- This parser supports their parsing.
maybeSemanticType :: Parser (Maybe SemanticType)
maybeSemanticType = Just <$> semanticType
