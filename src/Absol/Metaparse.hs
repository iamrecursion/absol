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
    -- ( 
    --     parseMetaspecFile
    -- ) where
        where

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Absol.Metaparse.Utilities
import           Control.Monad (void)
import           Data.Text 
import           Text.Megaparsec
-- import           Text.Megaparsec.Char
-- import           Text.Megaparsec.Expr
-- import           Text.Megaparsec.Perm
import           Text.Megaparsec.Text  (Parser)

-- TODO Add contextually sensitive parsing for keywords
-- TODO check that using keywords are in the list of allowed ones
-- TODO strip whitespace at the lowest level possible (requires refactor)
-- TODO refactor to flatten the AST for simpler traversal
-- TODO stateful parsing of types (checked) identifiers and expressions
-- TODO work out how to deal with types that haven't been imported

parseMetaspecFile :: Text -> IO ()
parseMetaspecFile = parseTest parseMetaspec

parseMetaspec :: Parser Metaspec
parseMetaspec = between spaceConsumer eof metaspec

-- TODO Statefully fail if missing any top-level defs
metaspec :: Parser Metaspec
metaspec = do
    defs <- some metaspecDefblock
    return $ Metaspec defs

metaspecDefblock :: Parser MetaspecDefblock
metaspecDefblock = do
    block <- nameDefblock 
        <|> versionDefblock 
        <|> usingDefblock
        -- <|> truthsDefblock
        -- <|> languageDefblock
    void ruleTerminationSymbol
    return block

nameDefblock :: Parser MetaspecDefblock
nameDefblock = do
    keywordWhere "name"
    name <- some nonSemicolon
    return (NameDefblock $ trimString name)

versionDefblock :: Parser MetaspecDefblock
versionDefblock = do
    keywordWhere "version"
    version <- some nonSemicolon
    return (VersionDefblock $ trimString version)

-- TODO error if these aren't in the list
usingDefblock :: Parser MetaspecDefblock
usingDefblock = do
    keywordWhere "using"
    items <- semanticBlock $ metaspecFeature `sepBy` multilineListSep
    return (UsingDefblock items)

metaspecFeature :: Parser MetaspecFeature
metaspecFeature = some (alphaNumChar <|> oneOf allowedSeps)
    where
        allowedSeps = "_-" :: String

truthsDefblock :: Parser MetaspecDefblock
truthsDefblock = do
    keywordWhere "truths"
    items <- semanticBlock semanticEvaluationList
    return (TruthsDefblock items)

languageDefblock :: Parser MetaspecDefblock
languageDefblock = do
    keywordWhere "language"
    semanticBlock languageDefinition

languageDefinition :: Parser MetaspecDefblock
languageDefinition = do
    productions1 <- languageRule `sepBy` spaceConsumer
    start <- startRule <* spaceConsumer
    productions2 <- languageRule `sepBy` spaceConsumer
    return (LanguageDefblock start (productions1 ++ productions2))

languageRule :: Parser LanguageRule
languageRule = do
    prodName <- nonTerminal identifier <* spaceConsumer
    void definingSymbol
    ruleBody <- languageRuleBody
    return (LanguageRule prodName ruleBody)

startRule :: Parser StartRule
startRule = do
    startSym <- startSymbol identifier <* spaceConsumer
    void definingSymbol
    ruleBody <- languageRuleBody 
    return (StartRule startSym ruleBody)

-- TODO consume whitespace to the end
languageRuleBody :: Parser LanguageRuleBody
languageRuleBody = do
    syntaxExpr <- syntaxExpression
    void ruleTerminationSymbol
    return (LanguageRuleBody syntaxExpr)

syntaxExpression :: Parser SyntaxExpression
syntaxExpression = do
    alternatives <- syntaxAlternative `sepBy1` multilineAlternative
    return (SyntaxExpression alternatives)

syntaxAlternative :: Parser SyntaxAlternative
syntaxAlternative = do
    terms <- syntaxTerm `sepBy1` space
    semantics <- languageRuleSemantics
    return (SyntaxAlternative terms semantics)

syntaxTerm :: Parser SyntaxTerm
syntaxTerm = do
    factor <- syntaxFactor
    exception <- option Nothing syntaxException
    return (SyntaxTerm factor exception)

syntaxException :: Parser (Maybe SyntaxException)
syntaxException = do
    void exceptSymbol
    term <- syntaxFactor
    return (Just $ SyntaxException term)

syntaxFactor :: Parser SyntaxFactor
syntaxFactor = do
    repeatChar <- option Nothing repeatSyntax
    prim <- syntaxPrimary
    return (SyntaxFactor repeatChar prim)

repeatSyntax :: Parser (Maybe RepeatSyntax)
repeatSyntax = do
    repeatCount <- naturalNumber
    void repeatCountSymbol
    return (Just $ RepeatSyntax repeatCount)

syntaxPrimary :: Parser SyntaxPrimary
syntaxPrimary = syntaxOptional
    <|> syntaxRepeated
    <|> syntaxGrouped
    <|> syntaxSpecial
    <|> terminalProxy
    <|> nonTerminalProxy
    <|> syntaxEmpty

syntaxOptional :: Parser SyntaxPrimary
syntaxOptional = do
    void optionalStartSymbol
    expr <- try syntaxExpression
    void optionalEndSymbol
    return (SyntaxOptional expr)

syntaxRepeated :: Parser SyntaxPrimary
syntaxRepeated = do
    void repeatStartSymbol
    expr <- syntaxExpression
    void optionalEndSymbol
    return (SyntaxRepeated expr)

syntaxGrouped :: Parser SyntaxPrimary
syntaxGrouped = do
    void groupStartSymbol
    expr <- syntaxExpression
    void groupEndSymbol
    return (SyntaxGrouped expr)

syntaxSpecial :: Parser SyntaxPrimary
syntaxSpecial = do
    void specialSequenceStartSymbol
    specialExpr <- parseString
    void specialSequenceEndSymbol
    return (SyntaxSpecial $ trimString specialExpr)

terminalProxy :: Parser SyntaxPrimary
terminalProxy = undefined

nonTerminalProxy :: Parser SyntaxPrimary
nonTerminalProxy = undefined

syntaxEmpty :: Parser SyntaxPrimary
syntaxEmpty = undefined

-- TODO make this parse !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
languageRuleSemantics :: Parser (Maybe LanguageRuleSemantics)
languageRuleSemantics = return Nothing

semanticEvaluationList :: Parser SemanticEvaluationList
semanticEvaluationList = semanticEvaluation `sepBy1` multilineListSep

semanticEvaluation :: Parser SemanticEvaluation
semanticEvaluation = semanticBlock semanticEvaluationBody

semanticEvaluationBody :: Parser SemanticEvaluation
semanticEvaluationBody = undefined

semanticType :: Parser SemanticType
semanticType = undefined
