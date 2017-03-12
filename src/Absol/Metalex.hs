-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metalex
-- Description : The implementation of the metaspec lexing engine.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
-- 
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
-- 
-- The primitive functions for lexing Metaspec files.
--
-------------------------------------------------------------------------------
module Absol.Metalex where

import           Absol.Metaparse.Grammar
import           Absol.Metaparse.Parser
import           Control.Monad           (void, when)
import           Data.List               (isInfixOf)
import           Data.Text               (pack)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer   as L

-- | Strips comments and whitespace from the input.
-- 
-- The definitions for whitespace and comments are specified in the metaspec 
-- grammar.
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "(*" "*)"

-- | This function provides a wrapper for lexeme parsers.
-- 
-- It specifies how to consume whitespace after each lexeme. 
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parses the provided terminal symbol of the language.
terminal :: String -> Parser String
terminal = L.symbol spaceConsumer

-- | Parses an unsigned numeric natural number literal. 
naturalNumber :: Parser Integer
naturalNumber = lexeme L.integer

-- | Parses a signed integer literal.
integer :: Parser Integer
integer = L.signed spaceConsumer naturalNumber

-- | Parses a language keyword.
keyword :: String -> Parser ()
keyword word = string word *> notFollowedBy illegals *> spaceConsumer
    where
        illegals = alphaNumChar

-- | Determines if an identifier is a reserved word.
-- 
-- TODO Need to have this take inputs instead, as these change based on parse
-- state. This is fine for now though.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many identifierChar
        check x = if
            | x `elem` semanticTypeList -> failExpr x
            | x `elem` semanticSpecialSyntaxList -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ "keyword " ++ show x ++ " cannot be an identifier."

semanticIdentifier :: Parser SemanticIdentifier
semanticIdentifier = SemanticIdentifier <$> identifier

-- TODO update metaspec grammar to reflect this
-- | Parses any character allowed in an identifier.
identifierChar :: Parser Char
identifierChar = alphaNumChar <|> oneOf seps
    where
        seps = "_-" :: String

-- | Parses a string allowed as a terminal.
-- 
-- Terminals may contain any characters except literal newline characters.
terminalString :: Parser TerminalString
terminalString = do
    str <- stringLiteral
    let check x = ("\n" `isInfixOf` x) || ("\r" `isInfixOf` x)
    when (check str) (fail "Literals may not contain newlines (\"\\n\\r\").")
    return (TerminalString str)

-- | Parses a string literal.
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* spaceConsumer

-- | Parses semantic types.
-- 
-- Disallows recognising the environment symbol 'e' as a type.
semanticTypeString :: Parser String
semanticTypeString = try (p >>= check) 
    where
        p = many identifierChar <* spaceConsumer
        check x = if
            | x == "e" -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ show x ++ " is not a valid type."

-- | Parses a non-terminal name.
nonTerminalIdentifier :: Parser NonTerminalIdentifier
nonTerminalIdentifier =
    NonTerminalIdentifier <$> ( (:) <$> letterChar <*> many identifierChar )

-- | Parses the start symbol for a non-terminal.
nonTerminalStart :: Parser NonTerminalStart
nonTerminalStart = terminal "<"

-- | Parses the end symbol for a non-terminal.
nonTerminalEnd :: Parser NonTerminalEnd
nonTerminalEnd = terminal ">"

-- | Parses the delimiters for a non-terminal symbol.
nonTerminalDelim :: Parser a -> Parser a
nonTerminalDelim = between nonTerminalStart nonTerminalEnd

-- | Parses the repeat count symbol.
repeatCountSymbol :: Parser RepeatCountSymbol
repeatCountSymbol = terminal "*"

-- | Parses the grammar except symbol.
exceptSymbol :: Parser ExceptSymbol
exceptSymbol = terminal "-"

-- | Parses the grammar disjunction symbol.
disjunctionSymbol :: Parser DisjunctionSymbol
disjunctionSymbol = terminal "|"

-- | Parses the grammar defining symbol.
definingSymbol :: Parser DefiningSymbol
definingSymbol = terminal "::="

-- | Parses the global rule termination symbol.
ruleTerminationSymbol :: Parser RuleTerminationSymbol
ruleTerminationSymbol = terminal ";"

-- | Parses the start symbol for an optional syntactic block.
optionalStartSymbol :: Parser OptionalStartSymbol
optionalStartSymbol = terminal "["

-- | Parses the end symbol for an optional syntactic block.
optionalEndSymbol :: Parser OptionalEndSymbol
optionalEndSymbol = terminal "]"

-- | Parses an optional syntactic block.
grammarOptional :: Parser a -> Parser a
grammarOptional = between optionalStartSymbol optionalEndSymbol

-- | Parses the start symbol for a syntactic group.
groupStartSymbol :: Parser GroupStartSymbol
groupStartSymbol = terminal "("

-- | Parses the end symbol for a syntactic group.
groupEndSymbol :: Parser GroupEndSymbol
groupEndSymbol = terminal ")"

-- | Parses a syntactic group.
grammarGroup :: Parser a -> Parser a
grammarGroup = between groupStartSymbol groupEndSymbol

-- | Parses the start symbol for a repeated syntactic block.
repeatStartSymbol :: Parser RepeatStartSymbol
repeatStartSymbol = terminal "{"

-- | Parses the end symbol for a repeated syntactic block.
repeatEndSymbol :: Parser RepeatEndSymbol
repeatEndSymbol = terminal "}"

-- | Parses a repeated syntactic block.
grammarRepeat :: Parser a -> Parser a
grammarRepeat = between repeatStartSymbol repeatEndSymbol

-- | Parses the start symbol for a special sequence block.
specialSequenceStartSymbol :: Parser SpecialSequenceStartSymbol
specialSequenceStartSymbol = terminal "<?"

-- | Parses the end symbol for a special sequence block.
specialSequenceEndSymbol :: Parser SpecialSequenceEndSymbol
specialSequenceEndSymbol = terminal "?>"

-- | Parses a special sequence block.
grammarSpecialSequence :: Parser a -> Parser a
grammarSpecialSequence = 
    between specialSequenceStartSymbol specialSequenceEndSymbol

-- | Parses the start delimiter of the grammar start symbol.
startSymbolStart :: Parser StartSymbolStart
startSymbolStart = terminal "<<"

-- | Parses the end delimiter of the grammar start symbol.
startSymbolEnd :: Parser StartSymbolEnd
startSymbolEnd = terminal ">>"

-- | Parses the delimiters for the grammar start symbol.
startSymbolDelim :: Parser a -> Parser a
startSymbolDelim = between startSymbolStart startSymbolEnd

-- | Parses the semantic behaviour symbol.
semanticBehavesAs :: Parser SemanticBehavesAs
semanticBehavesAs = terminal "-->"

-- | Parses the evaluates-to symbol.
evaluatesTo :: Parser EvaluatesTo
evaluatesTo = terminal "<="

-- | Parses the semantic where symbol.
whereSymbol :: Parser WhereSymbol
whereSymbol = terminal ":"

-- | Parses the semantic conjunction symbol.
semanticAnd :: Parser SemanticAnd
semanticAnd = terminal ","

-- | Parses the semantic assignment symbol.
semanticAssign :: Parser SemanticAssign
semanticAssign = terminal "="

-- | Parses the semantic environment symbol.
semanticEnvironmentSymbol :: Parser SemanticEnvironmentSymbol
semanticEnvironmentSymbol = terminal "e"

-- | Parses the semantic environment input symbol.
semanticEnvironmentInputSymbol :: Parser SemanticEnvironmentInputSymbol
semanticEnvironmentInputSymbol = terminal "<--"

-- | Parses the semantic environment access symbol.
environmentAccessSymbol :: Parser EnvironmentAccessSymbol
environmentAccessSymbol = terminal "."

-- | Parses the environment defines symbol.
environmentDefinesSymbol :: Parser EnvironmentDefinesSymbol
environmentDefinesSymbol = terminal ":"

-- | Parses the semantic list delimiter.
semanticListDelimiter :: Parser SemanticListDelimiter
semanticListDelimiter = terminal ","

-- | Parses the semantic disjunction symbol.
semanticDisjunction :: Parser SemanticDisjunction
semanticDisjunction = terminal "|"

-- | Parses the semantic block start symbol.
semanticBlockStart :: Parser SemanticBlockStart
semanticBlockStart = terminal "{"

-- | Parses the semantic block end symbol.
semanticBlockEnd :: Parser SemanticBlockEnd
semanticBlockEnd = terminal "}"

-- | Parses a semantic block.
semanticBlock :: Parser a -> Parser a
semanticBlock = between start end
    where
        start = spaceConsumer *> semanticBlockStart
        end = spaceConsumer *> semanticBlockEnd

-- | Parses the restriction block start symbol.
restrictionBlockStart :: Parser RestrictionBlockStart
restrictionBlockStart = terminal "("

-- | Parses the restriction block end symbol.
restrictionBlockEnd :: Parser RestrictionBlockEnd
restrictionBlockEnd = terminal ")"

-- | Parses a semantic restriction block.
restrictionBlock :: Parser a -> Parser a
restrictionBlock = between restrictionBlockStart restrictionBlockEnd

-- | Parses the syntax access index start symbol.
syntaxAccessStartSymbol :: Parser SyntaxAccessStartSymbol
syntaxAccessStartSymbol = terminal "["

-- | Parses the syntax access index end symbol.
syntaxAccessEndSymbol :: Parser SyntaxAccessEndSymbol
syntaxAccessEndSymbol = terminal "]"

-- | Parses a syntax access index block.
syntaxAccess :: Parser a -> Parser a
syntaxAccess = between syntaxAccessStartSymbol syntaxAccessEndSymbol

-- | Parses the special syntax block start symbol.
specialSyntaxStart :: Parser SpecialSyntaxStart
specialSyntaxStart = terminal "("

-- | Parses the special syntax block end symbol.
specialSyntaxEnd :: Parser SpecialSyntaxEnd
specialSyntaxEnd = terminal ")"

-- | Parses a special syntax block.
specialSyntaxBlock :: Parser a -> Parser a
specialSyntaxBlock = between specialSyntaxStart specialSyntaxEnd

-- | Parses a non-semicolon character.
nonSemicolon :: Parser Char
nonSemicolon = let 
        semi = ";" :: String
    in
        noneOf semi

-- | Parses a non-space character (ASCII).
nonSpace :: Parser Char
nonSpace = let
        mySpace = " " :: String
    in
        noneOf mySpace

-- | Parses any non-empty character.
nonEmptyChar :: Parser Char
nonEmptyChar = let
        emptyStr = "" :: String
    in
        noneOf emptyStr

-- | Parses a list separator ',' over multiple lines.
multilineListSep :: Parser String
multilineListSep =  try parseExpr
    where
        parseExpr = spaceConsumer *> semanticListDelimiter <* spaceConsumer

-- | Parses the semantic alternative '|' over multiple lines.
multilineAlternative :: Parser String
multilineAlternative = try parseExpr
    where
        parseExpr = spaceConsumer *> semanticDisjunction <* spaceConsumer

-- | Parses an operator terminal.
-- 
-- This supports a more sophisticated mechanism than 'terminal', allowing for an
-- operator to be a prefix of another operator without issue.
operator :: String -> Parser String
operator n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

-- | Parses a single, standard, open parenthesis. 
openParenthesis :: Parser OpenParenthesis
openParenthesis = terminal "("

-- | Parses a single, standard, close parenthesis.
closeParenthesis :: Parser CloseParenthesis
closeParenthesis = terminal ")"

-- | Parses a block surrounded by parentheses.
parentheses :: Parser a -> Parser a
parentheses = between openParenthesis closeParenthesis
