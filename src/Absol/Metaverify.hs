-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaverify
-- Description : Functions for the verification of metaspec semantics.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This file contains functions for performing the semantic verification process
-- on the AST of a metaspec file. 
--
-------------------------------------------------------------------------------
module Absol.Metaverify 
    (   
        verifyLanguage
    ) where

import           Absol.Metaparse.Grammar
import           Absol.Metaverify.Collate
import           Absol.Metaverify.RuleTag
import           Absol.Metaverify.State
import           Data.Map                 as M

import Debug.Trace

-- | Verifies the input language.
-- 
-- If the language can be proven complete, this returns True. In the case where
-- that property cannot be shown, an error string is returned, describing the
-- nature of the error.
verifyLanguage :: Metaspec -> Either Bool String
verifyLanguage x = case runState runVerification (collateASTData x) of
    (True, _) -> Left True
    (False, st) -> Right $ failString st
    where
        failString _ = "Failure to verify." -- TODO

runVerification :: VState Bool
runVerification = do 
    (tag, initRule) <- gets startRule
    (startRuleResult, _) <- verifyRule $ return initRule
    modify (updateStartRuleTag startRuleResult)
    case startRuleResult of
        Terminates -> return True
        _ -> return False

-- | Verifies a language rule.
-- 
-- It will trace the current verification path in case of error.
verifyRule :: VState LanguageRuleBody -> VState (RuleTag, [NonTerminal])
verifyRule rule = do
    (LanguageRuleBody expr) <- rule
    let (SyntaxExpression alternatives) = expr
        subTerms = (verifyAlternative . return) <$> alternatives
    return (Terminates, [])

-- | Verifies a syntax alternative.
-- 
-- Each syntax alternative can have its semantics verified independently, so 
-- it is simple to verify these in one go.
verifyAlternative :: VState SyntaxAlternative -> VState (RuleTag, [NonTerminal])
verifyAlternative alt = do
    alternative <- alt
    if hasSemantics alternative then
        verifyDefinedSemantics alt
    else
        verifySubSemantics alt

-- | Verifies a syntax alternative where the semantics are defined by hand.
verifyDefinedSemantics
    :: VState SyntaxAlternative
    -> VState (RuleTag, [NonTerminal])
verifyDefinedSemantics = undefined

-- | Verifies a syntax alternative where the semantics are composed indirectly.
verifySubSemantics
    :: VState SyntaxAlternative
    -> VState (RuleTag, [NonTerminal])
verifySubSemantics = undefined

hasSemantics :: SyntaxAlternative -> Bool
hasSemantics (SyntaxAlternative _ lrs) = case lrs of
    Just _ -> True
    Nothing -> False
