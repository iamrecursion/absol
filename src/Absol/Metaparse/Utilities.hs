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
        checkKeys
    ) where

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Control.Monad (void)
import           Data.Text 
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm
import           Text.Megaparsec.Text  (Parser)

keywordWhere :: String -> Parser ()
keywordWhere kwd = do
    keyword kwd
    void whereSymbol

checkKeys :: [MetaspecFeature] -> Parser ()
checkKeys = undefined
