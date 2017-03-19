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
import           Absol.Metaspec.Special
import           Control.Monad (void)
import           Data.List (intercalate)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Expr

-------------------------------------------------------------------------------

-- TODO General:
--      + Update EBNF grammar to reflect these changes.

-------------------------------------------------------------------------------

-- | Parses a metaspec file.
-- 
-- The file is taken as input and the corresponding parse-tree or error state is
-- returned. 
parseMetaspecFile :: Text -> IO ()
parseMetaspecFile = parseTest (runStateT parseMetaspec initParserState)

-- | Parses the top-level metaspec language definition.
parseMetaspec :: ParserST Metaspec
parseMetaspec = between spaceConsumer eof metaspec >>= check
    where
        check x = do
            result <- checkNTsInLang
            case result of
                Left ntList -> failExpr ntList
                Right _ -> return x
        failExpr x = fail $ str ++ ntStrings x ++ ". " ++ suggestion x 
        ntStrings x = intercalate ", " $ map extract x
        extract (NonTerminalIdentifier i) = "<" ++ i ++ ">"
        str = "The following Non-Terminals are used but not defined: "
        suggestion x = "Some may be defined in: " ++ featureStrings x ++ "."
        featuresForNTs x = filter whereValid $ findFeatureForNT <$> x
        featureList x = concat $ fromJust <$> featuresForNTs x
        featureStrings x = intercalate ", " $ toFeatureName <$> featureList x
        whereValid x = case x of
            Nothing -> False
            Just _ -> True

-- | Parses the top level metaspec definition blocks. 
-- 
-- The top-level definitions are parsed in the specified order, ensuring that
-- contextual information is provided in order. 
metaspec :: ParserST Metaspec
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
nameDefblock :: ParserST MetaspecDefblock
nameDefblock = do
    keywordWhere "name"
    name <- some nonSemicolon
    return (NameDefblock $ trimString name)

-- | Parses the language version definition block.
-- 
-- It strips whitespace from the start and end of the provided version string. 
versionDefblock :: ParserST MetaspecDefblock
versionDefblock = do
    keywordWhere "version"
    version <- some nonSemicolon
    return (VersionDefblock $ trimString version)

-- | Parses the using definition block for language features.
--
-- The using list is separated by ','.
usingDefblock :: ParserST MetaspecDefblock
usingDefblock = do
    keywordWhere "using"
    items <- semanticBlock $ metaspecFeature `sepBy` multilineListSep
    modify (updateImportedFeatures items)
    return (UsingDefblock items)

-- | Parses a language feature.
metaspecFeature :: ParserST MetaspecFeature
metaspecFeature = FeatureBase <$ terminal "base"
    <|> FeatureNumber <$ terminal "number"
    <|> FeatureString <$ terminal "string"
    <|> FeatureList <$ terminal "list"
    <|> FeatureMatrix <$ terminal "matrix"
    <|> FeatureTraverse <$ terminal "traverse"
    <|> FeatureFuncall <$ terminal "funcall"

-- | Parses the list of language truths.
-- 
-- These are used as termination cases by the termination proof engine for the
-- language. The list of truths is delimited by ','.
truthsDefblock :: ParserST MetaspecDefblock
truthsDefblock = do
    keywordWhere "truths"
    items <- semanticBlock semanticTruthsList
    return (TruthsDefblock items)

-- | Parses the language definition block.
languageDefblock :: ParserST MetaspecDefblock
languageDefblock = do
    keywordWhere "language"
    semanticBlock languageDefinition

-- | Parses the productions that define the language in metaspec.
-- 
-- The language start rule must be defined once in the file, and must be defined
-- before any other language productions in this block.
languageDefinition :: ParserST MetaspecDefblock
languageDefinition = do
    start <- startRule
    productions <- languageRule `sepBy` spaceConsumer
    return (LanguageDefblock start productions)

-- | Parses language productions.
languageRule :: ParserST LanguageRule
languageRule = do
    modify setPositionHead
    prodName <- nonTerminal
    void definingSymbol
    modify setPositionBody
    ruleBody <- languageRuleBody
    modify setPositionNone
    return (LanguageRule prodName ruleBody)

-- | Parses the language start rule.
startRule :: ParserST StartRule
startRule = do
    modify setPositionHead
    startSym <- startSymbol
    void definingSymbol
    modify setPositionBody
    ruleBody <- languageRuleBody
    modify setPositionNone
    return (StartRule startSym ruleBody)

-- | Parses the language start symbol. 
startSymbol :: ParserST StartSymbol
startSymbol = do
    ident <- startSymbolDelim nonTerminalIdentifier
    return (StartSymbol ident)

-- | Parses the body of a production.
-- 
-- The body of a production is the portion after the defining symbol.
languageRuleBody :: ParserST LanguageRuleBody
languageRuleBody = do
    syntaxExpr <- syntaxExpression
    void ruleTerminationSymbol
    return (LanguageRuleBody syntaxExpr)

-- | Parses a syntax expression.
-- 
-- Syntax expressions consist of the top-level alternatives for a given
-- production.
syntaxExpression :: ParserST SyntaxExpression
syntaxExpression = do
    alternatives <- syntaxAlternative `sepBy1` multilineAlternative
    return (SyntaxExpression alternatives)

-- | Parses each top-level alternative.
syntaxAlternative :: ParserST SyntaxAlternative
syntaxAlternative = do
    terms <- syntaxTerm `sepBy1` space
    semantics <- option Nothing languageRuleSemantics
    return (SyntaxAlternative terms semantics)

-- | Parses a syntactic term.
-- 
-- Syntactic terms may contain exception syntax. An exception is another syntax
-- definition that is subtracted from the set of allowed syntax defined by the
-- first production.
syntaxTerm :: ParserST SyntaxTerm
syntaxTerm = do
    factor <- syntaxFactor
    exception <- option Nothing $ try syntaxException
    return (SyntaxTerm factor exception)

-- | Parses the syntax exception itself.
syntaxException :: ParserST (Maybe SyntaxException)
syntaxException = do
    void exceptSymbol
    term <- syntaxFactor
    return (Just $ SyntaxException term)

-- | Parses syntactic factors.
-- 
-- These may optionally define a number of repetitions of the group.
syntaxFactor :: ParserST SyntaxFactor
syntaxFactor = do
    repeatChar <- option Nothing repeatSyntax
    prim <- syntaxPrimary
    return (SyntaxFactor repeatChar prim)

-- | Parses the optional repetition syntax. 
repeatSyntax :: ParserST (Maybe RepeatSyntax)
repeatSyntax = do
    repeatCount <- naturalNumber
    void repeatCountSymbol
    return (Just $ RepeatSyntax repeatCount)

-- | Parses the syntactic primary expressions.
syntaxPrimary :: ParserST SyntaxPrimary
syntaxPrimary = syntaxOptional
    <|> syntaxRepeated
    <|> syntaxGrouped
    <|> syntaxSpecial
    <|> terminalProxy
    <|> nonTerminalProxy

-- | Parses an optional piece of syntax.
syntaxOptional :: ParserST SyntaxPrimary
syntaxOptional = SyntaxOptional <$> grammarOptional syntaxExpression

-- | Parses a repeated syntax block.
syntaxRepeated :: ParserST SyntaxPrimary
syntaxRepeated = SyntaxRepeated <$> grammarRepeat syntaxExpression

-- | Parses a syntax group.
syntaxGrouped :: ParserST SyntaxPrimary
syntaxGrouped = SyntaxGrouped <$> grammarGroup syntaxExpression

-- | Parses a special syntax block.
-- 
-- Special syntax is used for language extensions and is currently not handled
-- by the metacompiler. Special expressions can contain any kind of text.
syntaxSpecial :: ParserST SyntaxPrimary
syntaxSpecial = do
    void specialSequenceStartSymbol
    specialExpr <- parseString
    void specialSequenceEndSymbol
    return (SyntaxSpecial $ trimString specialExpr)

-- | Parses a proxy for a terminal symbol of the language.
terminalProxy :: ParserST SyntaxPrimary
terminalProxy = TerminalProxy <$> parseTerminal

-- | Parses a proxy for the non-terminal symbol of the language.
nonTerminalProxy :: ParserST SyntaxPrimary
nonTerminalProxy = NonTerminalProxy <$> nonTerminal

-- | Parses a terminal symbol for the language.
parseTerminal :: ParserST Terminal
parseTerminal = Terminal <$> terminalString

-- | Parses a non-terminal symbol for the language.
nonTerminal :: ParserST NonTerminal
nonTerminal = do
    nt <- checkNTNotDefined $ nonTerminalDelim nonTerminalIdentifier
    modify $ addNTIdentifier nt
    return (NonTerminal nt)

-- | Parses the optional language rule semantics.
languageRuleSemantics :: ParserST (Maybe LanguageRuleSemantics)
languageRuleSemantics = do
    void semanticBehavesAs
    rules <- semanticBlock $ semanticRule `sepBy1` multilineAlternative
    return (Just $ LanguageRuleSemantics rules)

-- | Parses a semantic evaluation rule.
-- 
-- As the rules diverge only after consuming some portion of syntax, this parser
-- utilises the ability for infinite-lookahead backtracking to parse these 
-- productions.
-- 
-- TODO can I factor out the type checks? Nasty errors because of 'try' right
-- now.
semanticRule :: ParserST SemanticRule
semanticRule = try semanticEvaluationRule 
    <|> try environmentInputRule
    <|> try environmentAccessRuleProxy
    <|> specialSyntaxRuleProxy

-- | Parses an environment input rule.
--
-- Such rules deal with storing information (from the syntax) into the language
-- environment.
environmentInputRule :: ParserST SemanticRule
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
specialSyntaxRule :: ParserST SpecialSyntaxRule
specialSyntaxRule = do
    specialType <- semanticType
    specialOp <- semanticSpecialSyntax
    semanticBlocks <- specialSyntaxBlock $ 
        accessBlockOrRule `sepBy` multilineListSep
    return (SpecialSyntaxRule specialType specialOp semanticBlocks)

-- | Parses an environment access rule.
-- 
-- These rules are used to retrieve information that has been previously stored
-- in the semantic environment.
environmentAccessRule :: ParserST EnvironmentAccessRule
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
semanticEvaluationRule :: ParserST SemanticRule
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
environmentAccessRuleProxy :: ParserST SemanticRule
environmentAccessRuleProxy =
    EnvironmentAccessRuleProxy <$> environmentAccessRule

-- | Parses a proxy type for special syntax rules.
specialSyntaxRuleProxy :: ParserST SemanticRule
specialSyntaxRuleProxy = SpecialSyntaxRuleProxy <$> specialSyntaxRule

-- | Parses a syntax access block or environment access rule.
accessBlockOrRule :: ParserST AccessBlockOrRule
accessBlockOrRule = eitherP syntaxAccessBlock environmentAccessRule

-- | Parses a syntax access block or special syntax rule.
accessBlockOrSpecial :: ParserST AccessBlockOrSpecial
accessBlockOrSpecial = eitherP syntaxAccessBlock specialSyntaxRule

-- | Parses a syntax access blocks.
-- 
-- Such blocks are used within the semantics to refer to portions of syntax that
-- are defined in the grammar production.
syntaxAccessBlock :: ParserST SyntaxAccessBlock
syntaxAccessBlock = do
    nt <- nonTerminal
    address <- syntaxAccessor
    return (SyntaxAccessBlock nt address)

-- | Parses the syntax accessor address.
syntaxAccessor :: ParserST SyntaxAccessor
syntaxAccessor = SyntaxAccessor <$> syntaxAccess naturalNumber

-- | Parses a list of syntax access blocks.
syntaxAccessList :: ParserST SyntaxAccessList
syntaxAccessList = syntaxAccessBlock `sepBy` multilineListSep

-- | Parses a list of semantic truths.
-- 
-- Semantic truths are used to define the termination cases for the language 
-- semantics.
semanticTruthsList :: ParserST SemanticTruthsList
semanticTruthsList = semanticTruthBlock `sepBy` multilineListSep

-- | Parses a semantic truth block.
semanticTruthBlock :: ParserST SemanticTruth
semanticTruthBlock = semanticBlock semanticTruth

-- | Parses a semantic truth.
-- 
-- Each of these expressions is assumed to terminate by the proof engine.
semanticTruth :: ParserST SemanticTruth
semanticTruth = do
    semType <- semanticType
    semId <- semanticIdentifier
    void evaluatesTo
    nt <- nonTerminal
    return (SemanticTruth semType semId nt)

-- | Parses a list of semantic evaluations.
semanticEvaluationList :: ParserST SemanticEvaluationList
semanticEvaluationList = semanticEvaluation `sepBy1` multilineListSep

-- | Parses a semantic evaluation.
semanticEvaluation :: ParserST SemanticEvaluation
semanticEvaluation = semanticBlock semanticEvaluationBody

-- | Parses the body of a semantic evaluation expression.
-- 
-- These expressions are then composed to define the language rule semantics.
semanticEvaluationBody :: ParserST SemanticEvaluation
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
semanticOperationList :: ParserST SemanticOperationList
semanticOperationList = do
    let parseExpr = semanticOperationAssignment `sepBy1` multilineListSep
    operations <- semanticBlock parseExpr
    return (SemanticOperationList operations)

-- | Parses the evaluation operation of the semantics and its binding.
semanticOperationAssignment :: ParserST SemanticOperationAssignment
semanticOperationAssignment = do
    semId <- semanticIdentifier
    void semanticAssign
    semOp <- semanticOperation
    return (SemanticOperationAssignment semId semOp)

-- | Parses the allowed semantic combination operations.
semanticOperation :: ParserST SemanticOperation
semanticOperation = makeExprParser semanticExpression semanticOperatorTable

-- | Parses the allowed semantic combination expressions.
semanticExpression :: ParserST SemanticOperation
semanticExpression = parentheses semanticOperation
    <|> try variableAccess
    <|> Variable <$> semanticIdentifier
    <|> Constant <$> semanticValue

-- | Parses addressing to variables.
variableAccess :: ParserST SemanticOperation
variableAccess = do
    semId <- semanticIdentifier
    access <- parseAccessor
    return (VariableAccess semId access)
    where
        parseAccessor = parseListAccess <|> parseMatrixAccess
        parseListAccess = between (terminal "[") (terminal "]") accessList
        parseMatrixAccess = between (terminal "|") (terminal "|") accessList
        accessList = integer `sepBy` multilineListSep

-- | The operator table for the semantic operations.
-- 
-- Order in the top-level list defines precedence, and order in the sub-lists
-- defines parse order. 
semanticOperatorTable :: [[Operator ParserST SemanticOperation]]
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
            InfixL (InfixExpr Divide <$ operator "/"),
            InfixL (InfixExpr Modulo <$ operator "%")
        ],
        [
            InfixL (InfixExpr Plus <$ operator "+"),
            InfixL (InfixExpr Minus <$ operator "-"),
            InfixL (InfixExpr Cons <$ operator ":")
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
semanticSpecialSyntax :: ParserST SemanticSpecialSyntax
semanticSpecialSyntax = checkSpecialSyntaxAvailable parse
    where
        parse = SpecialSyntaxMap <$ specialSyntaxString "map"
            <|> SpecialSyntaxFold <$ specialSyntaxString "fold"
            <|> SpecialSyntaxFilter <$ specialSyntaxString "filter"
            <|> SpecialSyntaxDefproc <$ specialSyntaxString "defproc"
            <|> SpecialSyntaxDeffun <$ specialSyntaxString "deffun"
            <|> SpecialSyntaxCallproc <$ specialSyntaxString "callproc"
            <|> SpecialSyntaxCallfun <$ specialSyntaxString "callfun"

-- | Parses a list of semantic restrictions.
-- 
-- These restrictions allow control over which semantic behaviour is performed
-- conditional on the evaluation of the sub-terms.
semanticRestrictionList :: ParserST SemanticRestrictionList
semanticRestrictionList = do
    let parseExpr = semanticRestriction `sepBy` multilineListSep
    blocks <- restrictionBlock parseExpr
    return (SemanticRestrictionList blocks)

-- | Parses a semantic restriction.
semanticRestriction :: ParserST SemanticRestriction
semanticRestriction =
    makeExprParser semanticRestrictionExpr semanticRestrictionOperator

-- | Parses semantic restriction expressions.
semanticRestrictionExpr :: ParserST SemanticRestriction
semanticRestrictionExpr = SemVariable <$> semanticIdentifier
    <|> SemConstant <$> semanticValue

-- | Defines the operator table for the semantic restrictions.
-- 
-- All operators have the same precedence, with parse order defined by the order
-- in the list.
semanticRestrictionOperator :: [[Operator ParserST SemanticRestriction]]
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
semanticValue :: ParserST SemanticValue
semanticValue = semanticText
    <|> semanticNumber
    <|> semanticBoolean
    <|> semanticListLiteral
    <|> semanticMatrixLiteral

-- | Parses a list literal of the form [a, b, ...] or [].
semanticListLiteral :: ParserST SemanticValue
semanticListLiteral = 
    SemanticListLiteral <$> between (terminal "[") (terminal "]") parse
    where
        parse = (some nonSpace) `sepBy` multilineListSep

-- | Parses a matrix literal of the form |a, b; c, d| or ||.
semanticMatrixLiteral :: ParserST SemanticValue
semanticMatrixLiteral = 
    SemanticMatrixLiteral <$> between (terminal "|") (terminal "|") matrixValues
    where
        matrixValues = matrixRow `sepBy` (terminal ";")
        matrixRow = (some nonSpace) `sepBy` multilineListSep

-- | Parses a piece of constant semantic text.
semanticText :: ParserST SemanticValue
semanticText = do
    textVal <- stringLiteral
    return (SemanticText textVal)

-- | Parses a numerical constant.
semanticNumber :: ParserST SemanticValue
semanticNumber = SemanticNumber <$> integer

-- | Parses a boolean constant.
semanticBoolean :: ParserST SemanticValue
semanticBoolean = (terminal "true" *> pure (SemanticBoolean True)) 
    <|> (terminal "false" *> pure (SemanticBoolean False))

-- | Parses a semantic type expression.
--
-- This parser will only allow types that are in scope to be parsed, and will
-- raise an error if the type is not in scope or does not exist.
semanticType :: ParserST SemanticType
semanticType = checkTypeDefined parser
    where
        parser = AnyType <$ semanticTypeString "any"
            <|> NoneType <$ semanticTypeString "none"
            <|> BoolType <$ semanticTypeString "bool"
            <|> NaturalType <$ semanticTypeString "natural"
            <|> IntegerType <$ semanticTypeString "integer"
            <|> Int32Type <$ semanticTypeString "int32"
            <|> UInt32Type <$ semanticTypeString "uint32"
            <|> Int64Type <$ semanticTypeString "int64"
            <|> UInt64Type <$ semanticTypeString "uint64"
            <|> FloatType <$ semanticTypeString "float"
            <|> DoubleType <$ semanticTypeString "double"
            <|> IntegralType <$ semanticTypeString "integral"
            <|> FloatingType <$ semanticTypeString "floating"
            <|> NumberType <$ semanticTypeString "number"
            <|> StringType <$ semanticTypeString "string"
            <|> ListType <$ semanticTypeString "list"
            <|> MatrixType <$ semanticTypeString "matrix"

-- | Parses a semantic type that may not exist.
-- 
-- There are certain expressions where the inclusion of the type is optional.
-- This parser supports their parsing.
maybeSemanticType :: ParserST (Maybe SemanticType)
maybeSemanticType = Just <$> semanticType
