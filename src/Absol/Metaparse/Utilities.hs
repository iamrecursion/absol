-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse.Utilities
-- Description : Utility combinators for parsing Metaspec.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains additional parser combinators that do not parse the main
-- grammar of metaspec itself, but assist in making the main parser cleaner.
--
-------------------------------------------------------------------------------

module Absol.Metaparse.Utilities
    (   
        keywordWhere,
        checkKeys,
        trimString,
        parseString,
        syntaxEmpty
    ) where

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Absol.Metaparse.Parser
import           Control.Monad (void)
import           Text.Megaparsec

keywordWhere :: String -> Parser ()
keywordWhere kwd = do
    keyword kwd
    void whereSymbol

checkKeys :: [MetaspecFeature] -> Parser ()
checkKeys = undefined

trimString :: String -> String
trimString = unwords . words

parseString :: Parser String
parseString = some anyChar

syntaxEmpty :: Parser SyntaxPrimary
syntaxEmpty = do
    let parseExpr = terminal ")" 
            <|> terminal "]" 
            <|> terminal "}"
            <|> terminal "|"
            <|> terminal ";"
    void $ lookAhead $ try parseExpr
    return SyntaxEmpty
