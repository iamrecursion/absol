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

import           Absol.Metaparse.Grammar as G
import           Control.Monad           (void)
import           Data.Text
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer   as L
import           Text.Megaparsec.Text    (Parser)

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

-- | Parses an unsigned numeric integer literal. 
naturalNumber :: Parser Integer
naturalNumber = lexeme L.integer

integer :: Parser Integer
integer = L.signed spaceConsumer naturalNumber

keyword :: String -> Parser ()
keyword word = string word *> notFollowedBy illegals *> spaceConsumer
    where
        illegals = alphaNumChar
        -- allowed = "-_" :: String

-- | Determines if an identifier is a reserved word.
-- 
-- TODO Need to have this take inputs instead, as these change based on parse
-- state. This is fine for now though.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many identifierChar
        check x = if
            | t `elem` G.metaspecFeatureList -> failExpr x
            | t `elem` G.semanticTypeList -> failExpr x
            | t `elem` G.semanticSpecialSyntaxList -> failExpr x
            | otherwise -> return x
            where
                t = pack x
        failExpr x = fail $ "keyword " ++ show x ++ " cannot be an identifier."

-- TODO update metaspec grammar to reflect this
identifierChar :: Parser Char
identifierChar = alphaNumChar <|> oneOf seps
    where
        seps = "_-" :: String

-- TODO check if allowed
terminalString :: Parser String
terminalString = many terminalChar

terminalChar :: Parser Char
terminalChar = noneOf disallowed
    where
        disallowed = "\"\n\r" :: String

semanticTypeString :: Parser String
semanticTypeString = try (p >>= check) 
    where
        p = many identifierChar <* spaceConsumer
        check x = if
            | x == "e" -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ show x ++ " is not a valid type."

nonTerminalName :: Parser String
nonTerminalName = (:) <$> letterChar <*> many identifierChar

repeatCountSymbol :: Parser String
repeatCountSymbol = terminal "*"

exceptSymbol :: Parser String
exceptSymbol = terminal "-"

disjunctionSymbol :: Parser String
disjunctionSymbol = terminal "|"

definingSymbol :: Parser String
definingSymbol = terminal "::="

ruleTerminationSymbol :: Parser String
ruleTerminationSymbol = terminal ";"

optionalStartSymbol :: Parser String
optionalStartSymbol = terminal "["

optionalEndSymbol :: Parser String
optionalEndSymbol = terminal "]"

grammarOptional :: Parser a -> Parser a
grammarOptional = between optionalStartSymbol optionalEndSymbol

groupStartSymbol :: Parser String
groupStartSymbol = terminal "("

groupEndSymbol :: Parser String
groupEndSymbol = terminal ")"

grammarGroup :: Parser a -> Parser a
grammarGroup = between groupStartSymbol groupEndSymbol

repeatStartSymbol :: Parser String
repeatStartSymbol = terminal "{"

repeatEndSymbol :: Parser String
repeatEndSymbol = terminal "}"

grammarRepeat :: Parser a -> Parser a
grammarRepeat = between repeatStartSymbol repeatEndSymbol

specialSequenceStartSymbol :: Parser String
specialSequenceStartSymbol = terminal "<?"

specialSequenceEndSymbol :: Parser String
specialSequenceEndSymbol = terminal "?>"

grammarSpecialSequence :: Parser a -> Parser a
grammarSpecialSequence = 
    between specialSequenceStartSymbol specialSequenceEndSymbol

startSymbolStart :: Parser String
startSymbolStart = terminal "<<"

startSymbolEnd :: Parser String
startSymbolEnd = terminal ">>"

startSymbolDelim :: Parser a -> Parser a
startSymbolDelim = between startSymbolStart startSymbolEnd

nonTerminalStart :: Parser String
nonTerminalStart = terminal "<"

nonTerminalEnd :: Parser String
nonTerminalEnd = terminal ">"

nonTerminalDelim :: Parser a -> Parser a
nonTerminalDelim = between nonTerminalStart nonTerminalEnd

metaspecTerminalStart :: Parser String
metaspecTerminalStart = terminal "\""

metaspecTerminalEnd :: Parser String
metaspecTerminalEnd = terminal "\""

metaspecTerminalDelim :: Parser a -> Parser a
metaspecTerminalDelim = between metaspecTerminalStart metaspecTerminalEnd

semanticBehavesAs :: Parser String
semanticBehavesAs = terminal "-->"

evaluatesTo :: Parser String
evaluatesTo = terminal "<="

whereSymbol :: Parser String
whereSymbol = terminal ":"

semanticAnd :: Parser String
semanticAnd = terminal ","

semanticAssign :: Parser String
semanticAssign = terminal "="

semanticEnvironmentSymbol :: Parser String
semanticEnvironmentSymbol = terminal "e"

semanticEnvironmentInputSymbol :: Parser String
semanticEnvironmentInputSymbol = terminal "<--"

environmentAccessSymbol :: Parser String
environmentAccessSymbol = terminal "."

environmentDefinesSymbol :: Parser String
environmentDefinesSymbol = terminal ":"

semanticListDelimiter :: Parser String
semanticListDelimiter = terminal ","

semanticDisjunction :: Parser String
semanticDisjunction = terminal "|"

semanticBlockStart :: Parser String
semanticBlockStart = terminal "{"

semanticBlockEnd :: Parser String
semanticBlockEnd = terminal "}"

semanticBlock :: Parser a -> Parser a
semanticBlock = between start end
    where
        start = spaceConsumer *> semanticBlockStart
        end = spaceConsumer *> semanticBlockEnd

restrictionBlockStart :: Parser String
restrictionBlockStart = terminal "("

restrictionBlockEnd :: Parser String
restrictionBlockEnd = terminal ")"

restrictionBlock :: Parser a -> Parser a
restrictionBlock = between restrictionBlockStart restrictionBlockEnd

syntaxAccessStartSymbol :: Parser String
syntaxAccessStartSymbol = terminal "["

syntaxAccessEndSymbol :: Parser String
syntaxAccessEndSymbol = terminal "]"

syntaxAccess :: Parser a -> Parser a
syntaxAccess = between syntaxAccessStartSymbol syntaxAccessEndSymbol

specialSyntaxStart :: Parser String
specialSyntaxStart = terminal "("

specialSyntaxEnd :: Parser String
specialSyntaxEnd = terminal ")"

specialSyntaxBlock :: Parser a -> Parser a
specialSyntaxBlock = between specialSyntaxStart specialSyntaxEnd

nonSemicolon :: Parser Char
nonSemicolon = let 
        semi = ";" :: String
    in
        noneOf semi

nonSpace :: Parser Char
nonSpace = let
        mySpace = " " :: String
    in
        noneOf mySpace

nonEmptyChar :: Parser Char
nonEmptyChar = let
        emptyStr = "" :: String
    in
        noneOf emptyStr

multilineListSep :: Parser String
multilineListSep =  try parseExpr
    where
        parseExpr = spaceConsumer *> semanticListDelimiter <* spaceConsumer

multilineAlternative :: Parser String
multilineAlternative = try parseExpr
    where
        parseExpr = spaceConsumer *> semanticDisjunction <* spaceConsumer

operator :: String -> Parser String
operator n = (lexeme . try) (string n <* notFollowedBy punctuationChar)

openParenthesis :: Parser String
openParenthesis = terminal "("

closeParenthesis :: Parser String
closeParenthesis = terminal ")"

parentheses :: Parser a -> Parser a
parentheses = between openParenthesis closeParenthesis
