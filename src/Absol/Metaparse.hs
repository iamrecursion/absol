-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse
-- Description : Parser combinators for parsing Metaspec.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains the combinator parsers for parsing metaspec files. It 
-- provides robust error reporting functionality and efficient parser 
-- construction.
--
-------------------------------------------------------------------------------
module Absol.Metaparse 
    ( 
        parseMetaspecFile
    ) where

import Data.Text 
import Absol.Metaparse.Grammar
import Absol.Metalex
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm
import           Text.Megaparsec.Text  (Parser)

parseMetaspecFile :: Text -> IO ()
parseMetaspecFile = parseTest parseMetaspec

parseMetaspec :: Parser Metaspec
parseMetaspec = between spaceConsumer eof metaspec

metaspec :: Parser Metaspec
metaspec = undefined
