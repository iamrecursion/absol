-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaspec
-- Description : Functions for handling metaspec-specific language features.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- A module for working with metaspec language features. It is currently used to
-- re-export functions from Absol.Metaspec.Special.
--
-------------------------------------------------------------------------------
module Absol.Metaspec 
    (
        getTypes,
        getNonTerminals,
        getSpecialSyntax,
        providesNonTerminal,
        providesType,
        toFeatureName,
        findFeatureForNT,
        findFeatureForType,
        findFeatureForSpecial,
        extractNTIString,
        toTypeString,
        availableNonTerminals,
        availableTypes,
        toSpecialSyntaxName
    ) where

import Absol.Metaspec.Special
