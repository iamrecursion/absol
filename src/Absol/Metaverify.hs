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
-- To an extent it mirrors the structure of the parser. 
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
import           Control.Monad
import qualified Data.Map                 as M

import Debug.Trace

-- | Verifies the input language.
-- 
-- If the language can be proven complete, this returns True. In the case where
-- that property cannot be shown, an error string is returned, describing the
-- nature of the error.
verifyLanguage :: Metaspec -> Either Bool String
verifyLanguage x = case runState runVerification (collateASTData x) of
    (True, VerifierState _ prod _) -> do
        traceShowM $ fst <$> M.elems prod
        Left True
    (False, st) -> Right $ failString st
    where
        failString _ = "Failure to verify." -- TODO

runVerification :: VState Bool
runVerification = do 
    (_, initRule) <- gets startRule
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
    verifySyntaxExpr $ return expr

-- | Verifies a syntax expression.
verifySyntaxExpr :: VState SyntaxExpression -> VState (RuleTag, [NonTerminal])
verifySyntaxExpr expr = do
    (SyntaxExpression alternatives) <- expr
    let result = (verifyAlternative . return) <$> alternatives
    combineTerminationResults result

-- | Combines a set of subterm termination values into a result value for the 
-- term.
combineTerminationResults 
    :: [VState (RuleTag, [NonTerminal])]
    -> VState (RuleTag, [NonTerminal])
combineTerminationResults list = do
    items <- sequence list
    return $ combine items
    where
        combine :: [(RuleTag, [NonTerminal])] -> (RuleTag, [NonTerminal])
        combine = undefined

-- TODO can I use `ap` here? 

-- | Pulls result types out of the state monad.
-- extractFromState :: VState a -> a
extractFromState item = do
    st <- get
    evalState item st
 
-- | Verifies a syntax alternative.
-- 
-- Each syntax alternative can have its semantics verified independently, so 
-- it is simple to verify these in one go.
verifyAlternative :: VState SyntaxAlternative -> VState (RuleTag, [NonTerminal])
verifyAlternative alt = do
    alternative <- alt
    traceShowM alternative
    if hasSemantics alternative then
        verifyDefinedSemantics alt
    else
        verifySubSemantics alt

-- | Verifies a syntax alternative where the semantics are defined by hand.
verifyDefinedSemantics
    :: VState SyntaxAlternative
    -> VState (RuleTag, [NonTerminal])
verifyDefinedSemantics alt = do
    (SyntaxAlternative _ semantics) <- alt
    traceShowM semantics
    return (Terminates, [])

-- | Verifies a syntax alternative where the semantics are composed indirectly.
verifySubSemantics
    :: VState SyntaxAlternative
    -> VState (RuleTag, [NonTerminal])
verifySubSemantics alt = do
    (SyntaxAlternative terms _) <- alt 
    let _ = (verifySyntaxTerm . return) <$> terms
    return (Terminates, [])

-- | Verifies a syntax term.
-- 
-- Exceptions are treated as purely syntactic, and hence are not verified 
-- themselves.
verifySyntaxTerm :: VState SyntaxTerm -> VState (RuleTag, [NonTerminal])
verifySyntaxTerm term = do
    (SyntaxTerm factor _) <- term
    verifySyntaxFactor $ return factor

-- | Verifies a syntactic factor.
-- 
-- Repetition is purely a syntactic operation and is ignored here.
verifySyntaxFactor :: VState SyntaxFactor -> VState (RuleTag, [NonTerminal])
verifySyntaxFactor factor = do
    (SyntaxFactor _ primary) <- factor
    verifySyntaxPrimary $ return primary

-- | Verifies a syntax primary.
verifySyntaxPrimary :: VState SyntaxPrimary -> VState (RuleTag, [NonTerminal])
verifySyntaxPrimary primary = do
    syntaxPrimary <- primary
    case syntaxPrimary of
        (SyntaxOptional expr) -> verifySyntaxExpr $ return expr
        (SyntaxRepeated expr) -> verifySyntaxExpr $ return expr
        (SyntaxGrouped expr) -> verifySyntaxExpr $ return expr
        (SyntaxSpecial _) -> fail "Cannot verify special syntax."
        (TerminalProxy _) -> return (Terminates, [])
        (NonTerminalProxy nonTerminal) -> verifyNonTerminal $ return nonTerminal

-- | Verifies a given non-terminal. 
verifyNonTerminal :: VState NonTerminal -> VState (RuleTag, [NonTerminal])
verifyNonTerminal nt = do
    nonTerminal <- nt
    traceShowM nonTerminal
    return (Terminates, [])
    -- TODO map lookup and tag assignment

-- | Checks if a given language rule has explicitly defined semantics.
hasSemantics :: SyntaxAlternative -> Bool
hasSemantics (SyntaxAlternative _ lrs) = case lrs of
    Just _ -> True
    Nothing -> False
