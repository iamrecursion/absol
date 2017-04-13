-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Utilities
-- Description : Generic, useful utilities for the ABSOL project.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- A set of useful utility types, functions and definitions for the ABSOL
-- project.
--
-------------------------------------------------------------------------------

module Absol.Utilities where

import           Data.List (genericLength)

-- | The output token for certain diagnostic messages.
outputToken :: String
outputToken = ">> "

-- | Counts the number of occurrences of the input in the list.
countOccurrences :: Eq a => a -> [a] -> Integer
countOccurrences x = genericLength . filter (==x)
