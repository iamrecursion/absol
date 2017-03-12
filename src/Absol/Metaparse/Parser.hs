-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse.Parser
-- Description : Provides a stateful parser type for parsing metaspec. 
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a Parser with backtracking user-accessible state. 
--
-------------------------------------------------------------------------------
module Absol.Metaparse.Parser 
    (
        Parser(..)
    ) where

import           Control.Monad.State.Lazy (StateT)
import           Text.Megaparsec.Text  (Parser(..))

-- | Provides a parser type with backtracking user state.
type ParserST = StateT MetaState Parser

-- | The parser state.
data MetaState = MetaState {
    importedFeatures :: [String],
    definedNTs :: [String],
    keywordsInScope :: [String]
} deriving (Show) 
