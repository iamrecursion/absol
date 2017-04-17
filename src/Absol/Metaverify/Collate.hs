-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaverify.Collate
-- Description : Functions for organising the language AST.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- Functions for organising the language AST into the form needed by the form
-- needed to perform semantic verification.
--
-------------------------------------------------------------------------------
module Absol.Metaverify.Collate
    (
        collateASTData
    ) where

import           Absol.Metaparse.Grammar
import           Absol.Metaverify.State
import           Data.Maybe              (fromJust)

-- | Collates the data from the AST required for the verification algorithm.
collateASTData :: Metaspec -> VerifierState
collateASTData (Metaspec defblocks) =
    initVerifierState startRule langRules truths
    where
        languageDefblock = getDefblock defblocks isLanguageDefblock
        startRule = fromJust $ getStartRule languageDefblock
        langRules = fromJust $ getLanguageRules languageDefblock
        truths = fromJust $ getTruths $ getDefblock defblocks isTruthsDefblock

-- | Gets the start rule from the Language Defblock.
getStartRule :: MetaspecDefblock -> Maybe StartRule
getStartRule (LanguageDefblock start _) = Just start
getStartRule _                          = Nothing

-- | Gets the list of language productions from the Language Defblock.
getLanguageRules :: MetaspecDefblock -> Maybe [LanguageRule]
getLanguageRules (LanguageDefblock _ rules) = Just rules
getLanguageRules _                          = Nothing

-- | Gets the list of ground truths from the Truths Defblock.
getTruths :: MetaspecDefblock -> Maybe [SemanticTruth]
getTruths (TruthsDefblock truths) = Just truths
getTruths _                       = Nothing

-- | Gets the defblock specified by the provided function.
--
-- As each defblock must be defined once and only once, this is safe to call.
getDefblock
    :: [MetaspecDefblock]
    -> (MetaspecDefblock -> Bool)
    -> MetaspecDefblock
getDefblock xs fn = head $ filter fn xs

-- | Returns True if the provided defblock is a TruthsDefblock.
isTruthsDefblock :: MetaspecDefblock -> Bool
isTruthsDefblock (TruthsDefblock _) = True
isTruthsDefblock _                  = False

-- | Returns True if the provided defblock is a LanguageDefblock.
isLanguageDefblock :: MetaspecDefblock -> Bool
isLanguageDefblock (LanguageDefblock _ _) = True
isLanguageDefblock _                      = False
