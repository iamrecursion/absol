-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metagen.Utilities
-- Description : Utilities to support code generation in Metagen.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
-- 
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
-- 
-- Utilities supporting code generation in Metagen.
--
-------------------------------------------------------------------------------

module Absol.Metagen.Utilities (
        OutputFileNames(..),
        generateOutputFileNames
    ) where

import qualified Data.Text as T
import Data.Monoid ((<>))

-- | This type defines the output filenames for the code-generator.
data OutputFileNames = OutputFileNames {
    targetCompilerSource :: T.Text,
    targetCompilerBuildLog :: T.Text
} deriving (Eq, Show)

-- | Generates the set of output filenames for the code generator.
generateOutputFileNames :: String -> String -> OutputFileNames
generateOutputFileNames name ver = let
    namePart = T.pack $ name ++ "-" ++ ver
    compilerSrc = namePart <> T.pack "-compiler.hs"
    logFile = namePart <> T.pack "-build.log"
    in
        OutputFileNames compilerSrc logFile
