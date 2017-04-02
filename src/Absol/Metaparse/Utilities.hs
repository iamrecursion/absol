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
        trimString
    ) where

import           Absol.Metalex
import           Absol.Metaparse.Parser
import           Control.Monad (void)

-- | A utility parser for parsing a keyword followed by the where symbol.
keywordWhere :: String -> ParserST ()
keywordWhere kwd = do
    keyword kwd
    void whereSymbol

-- | Trims whitespace from the start and end of a string. 
trimString :: String -> String
trimString = unwords . words
