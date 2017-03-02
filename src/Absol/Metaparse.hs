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
    -- ( 
    --     parseMetaspecFile
    -- ) where
        where

import           Absol.Metalex
import           Absol.Metaparse.Grammar
import           Absol.Metaparse.Utilities
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
        <|> usingDefblock
        -- <|> truthsDefblock
        -- <|> languageDefblock
    void ruleTerminationSymbol
    return block

nameDefblock :: Parser MetaspecDefblock
nameDefblock = do
    keywordWhere "name"
    name <- some nonSemicolon
    return (NameDefblock name)

versionDefblock :: Parser MetaspecDefblock
versionDefblock = do
    keywordWhere "version"
    version <- some nonSemicolon
    return (VersionDefblock version)

usingDefblock :: Parser MetaspecDefblock
usingDefblock = do
    let
    keywordWhere "using"
    keys <- semanticBlock (metaspecFeature `sepBy` semanticListDelimiter)
    -- TODO check keys
    return (UsingDefblock keys)

metaspecFeature :: Parser MetaspecFeature
metaspecFeature = some (alphaNumChar <|> oneOf allowedSeps)
    where
        allowedSeps = "_-" :: String

truthsDefblock :: Parser MetaspecDefblock
truthsDefblock = undefined

languageDefblock :: Parser MetaspecDefblock
languageDefblock = undefined
