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

import           Control.Monad             (void)
import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm
import Text.Megaparsec.Char
import           Text.Megaparsec.Text      (Parser)
import qualified Text.Megaparsec.Lexer as L

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
