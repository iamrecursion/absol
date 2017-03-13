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
        ParserST,
        MetaState(..),
        initParserState,
        runStateT
    ) where

import           Absol.Metaparse.Grammar
import           Control.Monad.State.Lazy (StateT, runStateT)
import           Text.Megaparsec.Text     (Parser)

-- | Provides a parser type with backtracking user state.
type ParserST = StateT MetaState Parser

-- | The parser state.
data MetaState = MetaState {
    importedFeatures :: [MetaspecFeature],
    definedNTs :: [NonTerminalIdentifier],
    usedNTs :: [NonTerminalIdentifier]
} deriving (Show) 

-- | Generates an initial state for the parser.
initParserState :: MetaState
initParserState = MetaState [] [] []

-- TODO Accessor helper functions that wrap around put/get/modify
