-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaverify.Diagnostics
-- Description : Functions for pretty-printing language diagnostic messages.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This file contains functions to assist in pretty-printing the diagnostic 
-- messages produced by the verification engine. 
--
-------------------------------------------------------------------------------
module Absol.Metaverify.Diagnostics 
    (
        prettyPrintRuleTag,
        printLanguageDiagnostics
    ) where

import           Absol.Metaverify.RuleTag
import           Absol.Metaverify.State
import           Data.List                     (intercalate)
import qualified Data.Map                 as M

-- | Pretty prints the rule tag output for language diagnostics.
prettyPrintRuleTag :: RuleTag -> String
prettyPrintRuleTag Terminates = "Terminates."
prettyPrintRuleTag Touched = "Semantics unneeded, not verified."
prettyPrintRuleTag Untouched = "Production unused."
prettyPrintRuleTag (DoesNotTerminate xs) = let
        ntItems = printNonTerminationItem <$> xs
        itemString = intercalate "\n\n" ntItems
    in
        "Does not terminate.\n\n" ++ itemString

-- | Pretty prints a single item from the non-termination list.
printNonTerminationItem :: NonTerminationItem -> String
printNonTerminationItem (nonTermType, ntTrace, str) = let
        typeStr = case nonTermType of
            Diverges -> "Production diverges."
            Incomplete -> "Production incomplete."
            IncompleteGuards -> "Guards incomplete."
            IncorrectEvaluationForm -> "Incorrect Semantic Form."
            NonExistentSubterms -> "Refers to non-existent subterms."
            UnableToInfer -> "Unable to infer semantics for rule."
        makeTrace = intercalate " -> " $ show <$> ntTrace
    in 
        typeStr ++ "\n    REASON: " ++ str ++ "\n    IN: " ++ makeTrace

-- | Prints a summary diagnostic for the entire language.
printLanguageDiagnostics :: ProductionMap -> String
printLanguageDiagnostics productions = let 
        extractTag (x,_) = x
        ntTagPairs = 
            zip (M.keys productions) (extractTag <$> M.elems productions)
        ntString (nt, tag) = "----------\n\nPRODUCTION: " ++
            show nt ++ "\n" ++ "STATUS: " ++ prettyPrintRuleTag tag ++ "\n"
        ntStrings = intercalate "\n" $ ntString <$> ntTagPairs
    in
        "===== FULL LANGUAGE DIAGNOSTIC =====\nEach production was evaluated\ 
        \ as follows:\n\n" ++ ntStrings
