
{-# LANGUAGE MultiWayIf #-}

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
module Absol.Metaparse.Grammar (

    ) where

import           Control.Monad         (void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Perm
import           Text.Megaparsec.Text  (Parser)

-- Integer Double

-- A basic grammar experiment
-- aexp = identifier | integer | "-", aexp | aexp, opa, aexp ;
-- bexp = "true" | "false" | "not", bexp | bexp, opb, bexp | aexp, opr, aexp ;
-- opa = "+" | "*" | "-" | "/" ;
-- opb = "and" | "or" ;
-- opr = "<" | ">"
-- comm = identifier, ":=", aexp
--      | "skip"
--      | comm, ";", comm
--      | "(", comm, ")"
--      | "if", bexp, "then", comm, "else", comm
--      | "while", bexp, "do", comm

data BExpr
    = BoolConstant Bool
    | Not BExpr
    | BoolBinary BoolBinOp BExpr BExpr
    | RelBinary RelBinOp AExpr AExpr
    deriving (Show)

data AExpr
    = Identifier String
    | IntegerConstant Integer
    | NegateExpr AExpr
    | ArithBinary ArithBinOp AExpr AExpr
    deriving (Show)

data BoolBinOp = And | Or deriving (Show)

data RelBinOp = LessThan | GreaterThan deriving (Show)

data ArithBinOp = Add | Multiply | Subtract | Divide deriving (Show)

data Comm
    = Assignment String AExpr
    | Skip
    | Seq [Comm]
    | CondExpr BExpr Comm Comm
    | WhileExpr BExpr Comm
    deriving (Show)

-- LEXER CODE

-- | This function consumes whitespace and comments in the language
--
-- Whitespace and comments are defined as in the Metaspec grammar.
sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "(*" "*)"

-- | Determines the lexeme consumer strategy.
--
-- Space is consumed after lexemes, but not before them.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses the provided terminal symbol.
terminal :: String -> Parser String
terminal = L.symbol sc

parentheses :: Parser a -> Parser a
parentheses = between (terminal "(") (terminal ")")

integer :: Parser Integer
integer = lexeme L.integer

semi :: Parser String
semi = terminal ";"

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

reservedWordList :: [String]
reservedWordList =
    ["if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
        p = (:) <$> letterChar <*> many alphaNumChar
        check x = if
            | x `elem` reservedWordList -> failExpr x
            | otherwise -> return x
        failExpr x = fail $ "keyword " ++ show x ++ " cannot be an identifier."

-- PARSER CODE
parseLanguage :: Parser Comm
parseLanguage = between sc eof comm

comm :: Parser Comm
comm = parentheses comm <|> commSequence

comm' :: Parser Comm
comm' = ifStatement <|> whileStatement <|> skipStatement <|> assignStatement

commSequence :: Parser Comm
commSequence = f <$> sepBy1 comm' semi
    where
        f l = if length l == 1 then head l else Seq l

ifStatement :: Parser Comm
ifStatement = do
    reservedWord "if"
    cond <- booleanExpression
    reservedWord "then"
    statement1 <- comm
    reservedWord "else"
    statement2 <- comm
    return (CondExpr cond statement1 statement2)

whileStatement :: Parser Comm
whileStatement = do
    reservedWord "while"
    cond <- booleanExpression
    reservedWord "do"
    statement <- comm
    return (WhileExpr cond statement)

assignStatement :: Parser Comm
assignStatement = do
    varName <- identifier
    void (terminal ":=") -- ignore the result of evaluation
    expression <- arithmeticExpression
    return (Assignment varName expression)

skipStatement :: Parser Comm
skipStatement = Skip <$ reservedWord "skip"

booleanExpression :: Parser BExpr
booleanExpression = makeExprParser boolTerm boolOperators

arithmeticExpression :: Parser AExpr
arithmeticExpression = makeExprParser arithTerm arithOperators

-- Precedence given by order in the operators list
arithOperators :: [[Operator Parser AExpr]]
arithOperators =
    [
        [ Prefix (NegateExpr <$ terminal "-")],
        [
            InfixL (ArithBinary Multiply <$ terminal "*"),
            InfixL (ArithBinary Divide <$ terminal "*")
        ],
        [
            InfixL (ArithBinary Add <$ terminal "+"),
            InfixL (ArithBinary Subtract <$ terminal "-")
        ]
    ]

boolOperators :: [[Operator Parser BExpr]]
boolOperators =
    [
        [ Prefix (Not <$ reservedWord "not") ],
        [
            InfixL (BoolBinary And <$ reservedWord "and"),
            InfixL (BoolBinary Or <$ reservedWord "or")
        ]
    ]

arithTerm :: Parser AExpr
arithTerm = parentheses arithmeticExpression
    <|> Identifier <$> identifier
    <|> IntegerConstant <$> integer

boolTerm :: Parser BExpr
boolTerm = parentheses booleanExpression
    <|> (reservedWord "true" *> pure (BoolConstant True))
    <|> (reservedWord "false" *> pure (BoolConstant False))
    <|> relationalExpression

relationalExpression :: Parser BExpr
relationalExpression = do
    arg1 <- arithmeticExpression
    op <- relationOp
    arg2 <- arithmeticExpression
    return (RelBinary op arg1 arg2)

relationOp :: Parser RelBinOp
relationOp = (terminal ">" *> pure GreaterThan)
    <|> (terminal "<" *> pure LessThan)
