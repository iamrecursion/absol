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
module Absol.Metalex
    ( 
        spaceConsumer,
        lexeme,
        terminal
    ) where

import           Control.Monad         (void)
import           Data.Text
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

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


