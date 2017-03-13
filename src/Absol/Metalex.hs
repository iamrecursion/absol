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
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer   as L

-- | Strips comments and whitespace from the input.
-- 
-- The definitions for whitespace and comments are specified in the metaspec 
-- grammar.
spaceConsumer :: ParserST ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "(*" "*)"

spaceConsumer' :: ParserST ()
spaceConsumer' = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "(*" "*)"        

-- | This function provides a wrapper for lexeme parsers.
-- 
-- It specifies how to consume whitespace after each lexeme. 
lexeme :: ParserST a -> ParserST a
lexeme = L.lexeme spaceConsumer

-- | Parses the provided terminal symbol of the language.
terminal :: String -> ParserST String
terminal = L.symbol spaceConsumer

-- | Parses an unsigned numeric natural number literal. 
naturalNumber :: ParserST Integer
naturalNumber = lexeme L.integer

-- | Parses a signed integer literal.
integer :: ParserST Integer
integer = L.signed spaceConsumer naturalNumber

-- | Parses a language keyword.
keyword :: String -> ParserST ()
keyword word = string word *> notFollowedBy illegals *> spaceConsumer
    where
        illegals = alphaNumChar

-- | Determines if an identifier is a reserved word.
-- 
-- TODO Need to have this take inputs instead, as these change based on parse
-- state. This is fine for now though.
identifier :: ParserST String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many identifierChar
        check x = if
            | x `elem` semanticTypeList -> failExpr x
            | x `elem` semanticSpecialSyntaxList -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ "keyword " ++ show x ++ " cannot be an identifier."

semanticIdentifier :: ParserST SemanticIdentifier
semanticIdentifier = SemanticIdentifier <$> identifier

-- TODO update metaspec grammar to reflect this
-- | Parses any character allowed in an identifier.
identifierChar :: ParserST Char
identifierChar = alphaNumChar <|> oneOf seps
    where
        seps = "_-" :: String

-- | Parses a string allowed as a terminal.
-- 
-- Terminals may contain any characters except literal newline characters.
terminalString :: ParserST TerminalString
terminalString = do
    str <- stringLiteral
    let check x = ("\n" `isInfixOf` x) || ("\r" `isInfixOf` x)
    when (check str) (fail "Literals may not contain newlines (\"\\n\\r\").")
    return (TerminalString str)

-- | Parses a string literal.
stringLiteral :: ParserST String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"') <* spaceConsumer

-- | Parses semantic types.
-- 
-- Disallows recognising the environment symbol 'e' as a type.
semanticTypeString :: ParserST String
semanticTypeString = try (p >>= check) 
    where
        p = many identifierChar <* spaceConsumer
        check x = if
            | x == "e" -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ show x ++ " is not a valid type."

-- | Parses a non-terminal name.
nonTerminalIdentifier :: ParserST NonTerminalIdentifier
nonTerminalIdentifier =
    NonTerminalIdentifier <$> ( (:) <$> letterChar <*> many identifierChar )

-- | Parses the start symbol for a non-terminal.
nonTerminalStart :: ParserST NonTerminalStart
nonTerminalStart = terminal "<"

-- | Parses the end symbol for a non-terminal.
nonTerminalEnd :: ParserST NonTerminalEnd
nonTerminalEnd = terminal ">"

-- | Parses the delimiters for a non-terminal symbol.
nonTerminalDelim :: ParserST a -> ParserST a
nonTerminalDelim = between nonTerminalStart nonTerminalEnd

-- | Parses the repeat count symbol.
repeatCountSymbol :: ParserST RepeatCountSymbol
repeatCountSymbol = terminal "*"

-- | Parses the grammar except symbol.
exceptSymbol :: ParserST ExceptSymbol
exceptSymbol = terminal "-"

-- | Parses the grammar disjunction symbol.
disjunctionSymbol :: ParserST DisjunctionSymbol
disjunctionSymbol = terminal "|"

-- | Parses the grammar defining symbol.
definingSymbol :: ParserST DefiningSymbol
definingSymbol = terminal "::="

-- | Parses the global rule termination symbol.
ruleTerminationSymbol :: ParserST RuleTerminationSymbol
ruleTerminationSymbol = terminal ";"

-- | Parses the start symbol for an optional syntactic block.
optionalStartSymbol :: ParserST OptionalStartSymbol
optionalStartSymbol = terminal "["

-- | Parses the end symbol for an optional syntactic block.
optionalEndSymbol :: ParserST OptionalEndSymbol
optionalEndSymbol = terminal "]"

-- | Parses an optional syntactic block.
grammarOptional :: ParserST a -> ParserST a
grammarOptional = between optionalStartSymbol optionalEndSymbol

-- | Parses the start symbol for a syntactic group.
groupStartSymbol :: ParserST GroupStartSymbol
groupStartSymbol = terminal "("

-- | Parses the end symbol for a syntactic group.
groupEndSymbol :: ParserST GroupEndSymbol
groupEndSymbol = terminal ")"

-- | Parses a syntactic group.
grammarGroup :: ParserST a -> ParserST a
grammarGroup = between groupStartSymbol groupEndSymbol

-- | Parses the start symbol for a repeated syntactic block.
repeatStartSymbol :: ParserST RepeatStartSymbol
repeatStartSymbol = terminal "{"

-- | Parses the end symbol for a repeated syntactic block.
repeatEndSymbol :: ParserST RepeatEndSymbol
repeatEndSymbol = terminal "}"

-- | Parses a repeated syntactic block.
grammarRepeat :: ParserST a -> ParserST a
grammarRepeat = between repeatStartSymbol repeatEndSymbol

-- | Parses the start symbol for a special sequence block.
specialSequenceStartSymbol :: ParserST SpecialSequenceStartSymbol
specialSequenceStartSymbol = terminal "<?"

-- | Parses the end symbol for a special sequence block.
specialSequenceEndSymbol :: ParserST SpecialSequenceEndSymbol
specialSequenceEndSymbol = terminal "?>"

-- | Parses a special sequence block.
grammarSpecialSequence :: ParserST a -> ParserST a
grammarSpecialSequence = 
    between specialSequenceStartSymbol specialSequenceEndSymbol

-- | Parses the start delimiter of the grammar start symbol.
startSymbolStart :: ParserST StartSymbolStart
startSymbolStart = terminal "<<"

-- | Parses the end delimiter of the grammar start symbol.
startSymbolEnd :: ParserST StartSymbolEnd
startSymbolEnd = terminal ">>"

-- | Parses the delimiters for the grammar start symbol.
startSymbolDelim :: ParserST a -> ParserST a
startSymbolDelim = between startSymbolStart startSymbolEnd

-- | Parses the semantic behaviour symbol.
semanticBehavesAs :: ParserST SemanticBehavesAs
semanticBehavesAs = terminal "-->"

-- | Parses the evaluates-to symbol.
evaluatesTo :: ParserST EvaluatesTo
evaluatesTo = terminal "<="

-- | Parses the semantic where symbol.
whereSymbol :: ParserST WhereSymbol
whereSymbol = terminal ":"

-- | Parses the semantic conjunction symbol.
semanticAnd :: ParserST SemanticAnd
semanticAnd = terminal ","

-- | Parses the semantic assignment symbol.
semanticAssign :: ParserST SemanticAssign
semanticAssign = terminal "="

-- | Parses the semantic environment symbol.
semanticEnvironmentSymbol :: ParserST SemanticEnvironmentSymbol
semanticEnvironmentSymbol = terminal "e"

-- | Parses the semantic environment input symbol.
semanticEnvironmentInputSymbol :: ParserST SemanticEnvironmentInputSymbol
semanticEnvironmentInputSymbol = terminal "<--"

-- | Parses the semantic environment access symbol.
environmentAccessSymbol :: ParserST EnvironmentAccessSymbol
environmentAccessSymbol = terminal "."

-- | Parses the environment defines symbol.
environmentDefinesSymbol :: ParserST EnvironmentDefinesSymbol
environmentDefinesSymbol = terminal ":"

-- | Parses the semantic list delimiter.
semanticListDelimiter :: ParserST SemanticListDelimiter
semanticListDelimiter = terminal ","

-- | Parses the semantic disjunction symbol.
semanticDisjunction :: ParserST SemanticDisjunction
semanticDisjunction = terminal "|"

-- | Parses the semantic block start symbol.
semanticBlockStart :: ParserST SemanticBlockStart
semanticBlockStart = terminal "{"

-- | Parses the semantic block end symbol.
semanticBlockEnd :: ParserST SemanticBlockEnd
semanticBlockEnd = terminal "}"

-- | Parses a semantic block.
semanticBlock :: ParserST a -> ParserST a
semanticBlock = between start end
    where
        start = spaceConsumer *> semanticBlockStart
        end = spaceConsumer *> semanticBlockEnd

-- | Parses the restriction block start symbol.
restrictionBlockStart :: ParserST RestrictionBlockStart
restrictionBlockStart = terminal "("

-- | Parses the restriction block end symbol.
restrictionBlockEnd :: ParserST RestrictionBlockEnd
restrictionBlockEnd = terminal ")"

-- | Parses a semantic restriction block.
restrictionBlock :: ParserST a -> ParserST a
restrictionBlock = between restrictionBlockStart restrictionBlockEnd

-- | Parses the syntax access index start symbol.
syntaxAccessStartSymbol :: ParserST SyntaxAccessStartSymbol
syntaxAccessStartSymbol = terminal "["

-- | Parses the syntax access index end symbol.
syntaxAccessEndSymbol :: ParserST SyntaxAccessEndSymbol
syntaxAccessEndSymbol = terminal "]"

-- | Parses a syntax access index block.
syntaxAccess :: ParserST a -> ParserST a
syntaxAccess = between syntaxAccessStartSymbol syntaxAccessEndSymbol

-- | Parses the special syntax block start symbol.
specialSyntaxStart :: ParserST SpecialSyntaxStart
specialSyntaxStart = terminal "("

-- | Parses the special syntax block end symbol.
specialSyntaxEnd :: ParserST SpecialSyntaxEnd
specialSyntaxEnd = terminal ")"

-- | Parses a special syntax block.
specialSyntaxBlock :: ParserST a -> ParserST a
specialSyntaxBlock = between specialSyntaxStart specialSyntaxEnd

-- | Parses a non-semicolon character.
nonSemicolon :: ParserST Char
nonSemicolon = let 
        semi = ";" :: String
    in
        noneOf semi

-- | Parses a non-space character (ASCII).
nonSpace :: ParserST Char
nonSpace = let
        mySpace = " " :: String
    in
        noneOf mySpace

-- | Parses any non-empty character.
nonEmptyChar :: ParserST Char
nonEmptyChar = let
        emptyStr = "" :: String
    in
        noneOf emptyStr

-- | Parses a list separator ',' over multiple lines.
multilineListSep :: ParserST String
multilineListSep =  try parseExpr
    where
        parseExpr = spaceConsumer *> semanticListDelimiter <* spaceConsumer

-- | Parses the semantic alternative '|' over multiple lines.
multilineAlternative :: ParserST String
multilineAlternative = try parseExpr
    where
        parseExpr = spaceConsumer *> semanticDisjunction <* spaceConsumer

-- | Parses an operator terminal.
-- 
-- This supports a more sophisticated mechanism than 'terminal', allowing for an
-- operator to be a prefix of another operator without issue.
operator :: String -> ParserST String
operator n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

-- | Parses a single, standard, open parenthesis. 
openParenthesis :: ParserST OpenParenthesis
openParenthesis = terminal "("

-- | Parses a single, standard, close parenthesis.
closeParenthesis :: ParserST CloseParenthesis
closeParenthesis = terminal ")"

-- | Parses a block surrounded by parentheses.
parentheses :: ParserST a -> ParserST a
parentheses = between openParenthesis closeParenthesis
