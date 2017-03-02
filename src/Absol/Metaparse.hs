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

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Control.Monad (void)
import           Data.Text 
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Perm
import           Text.Megaparsec.Text  (Parser)

-- TODO Add contextually sensitive parsing for keywords
-- TODO check that using keywords are in the list of allowed ones

parseMetaspecFile :: Text -> IO ()
parseMetaspecFile = parseTest parseMetaspec

parseMetaspec :: Parser Metaspec
parseMetaspec = between spaceConsumer eof metaspec

-- TODO Statefully fail if missing any top-level defs
metaspec :: Parser Metaspec
metaspec = do
    defs <- some metaspecDefblock
    return $ Metaspec defs

metaspecDefblock :: Parser MetaspecDefblock
metaspecDefblock = do
    block <- nameDefblock 
        <|> versionDefblock 
        -- <|> usingDefblock
        -- <|> truthsDefblock
        -- <|> languageDefblock
    void ruleTerminationSymbol
    return block

nameDefblock :: Parser MetaspecDefblock
nameDefblock = do
    keyword "name"
    void whereSymbol
    name <- some nonSemicolon
    return (NameDefblock name)

versionDefblock :: Parser MetaspecDefblock
versionDefblock = do
    keyword "version"
    void whereSymbol
    version <- some nonSemicolon
    return (VersionDefblock version)

usingDefblock :: Parser MetaspecDefblock
usingDefblock = undefined

truthsDefblock :: Parser MetaspecDefblock
truthsDefblock = undefined

languageDefblock :: Parser MetaspecDefblock
languageDefblock = undefined
