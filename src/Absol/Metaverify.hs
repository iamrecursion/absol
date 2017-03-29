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
import           Absol.Metaparse.Utilities
import           Absol.Metaverify.Collate
import           Absol.Metaverify.RuleTag
import           Absol.Metaverify.State
import qualified Data.Map                 as M

import Debug.Trace

-- | Verifies the input language.
-- 
-- If the language can be proven complete, this returns True. In the case where
-- that property cannot be shown, an error string is returned, describing the
-- nature of the error.
-- 
-- It will also alert the user to any unused productions.
verifyLanguage :: Metaspec -> Either Bool String
verifyLanguage x = case runState runVerification (collateASTData x) of
    (True, VerifierState (tag, _) prod _) -> do
        traceShowM tag
        traceShowM $ zip (show <$> M.keys prod) (fst <$> M.elems prod)
        Left True
    (False, VerifierState (tag, _) prod _) -> do
        traceShowM tag
        traceShowM $ zip (show <$> M.keys prod) (fst <$> M.elems prod)
        Right $ failString tag
    where
        failString _ = "Failure to verify." -- TODO 

-- | Runs the verification process on the language rules.
-- 
-- It begins at the start rule and recurses through the productions of the 
-- language.
runVerification :: VState Bool
runVerification = do
    (_, initRule) <- gets startRule
    startRuleResult <- verifyRule $ return initRule
    modify (updateStartRuleTag startRuleResult)
    case startRuleResult of
        Terminates -> return True
        _ -> return False

-- | Verifies a language rule.
-- 
-- It will trace the current verification path in case of error.
verifyRule :: VState LanguageRuleBody -> VState RuleTag
verifyRule rule = do
    (LanguageRuleBody expr) <- rule
    verifySyntaxExpr $ return expr

-- | Verifies a syntax expression.
verifySyntaxExpr :: VState SyntaxExpression -> VState RuleTag
verifySyntaxExpr expr = do
    (SyntaxExpression alternatives) <- expr
    let result = (verifyAlternative . return) <$> alternatives
    res <- combineTerminationResults result
    return res
 
-- | Verifies a syntax alternative.
-- 
-- Each syntax alternative can have its semantics verified independently, so 
-- it is simple to verify these in one go.
verifyAlternative :: VState SyntaxAlternative -> VState RuleTag
verifyAlternative alt = do
    alternative <- alt
    if hasSemantics alternative then
        verifyDefinedSemantics alt
    else
        verifySubSemantics alt

-- | Verifies a syntax alternative where the semantics are defined by hand.
verifyDefinedSemantics :: VState SyntaxAlternative -> VState RuleTag
verifyDefinedSemantics alt = do
    (SyntaxAlternative _ semantics) <- alt
    return Terminates

-- | Verifies a syntax alternative where the semantics are composed indirectly.
verifySubSemantics :: VState SyntaxAlternative -> VState RuleTag
verifySubSemantics alt = do
    (SyntaxAlternative terms _) <- alt
    let result = (verifySyntaxTerm . return) <$> terms
    combineTerminationResults result

-- | Verifies a syntax term.
-- 
-- Exceptions are treated as purely syntactic, and hence are not verified 
-- themselves.
verifySyntaxTerm :: VState SyntaxTerm -> VState RuleTag
verifySyntaxTerm term = do
    (SyntaxTerm factor _) <- term
    verifySyntaxFactor $ return factor

-- | Verifies a syntactic factor.
-- 
-- Repetition is purely a syntactic operation and is ignored here.
verifySyntaxFactor :: VState SyntaxFactor -> VState RuleTag
verifySyntaxFactor factor = do
    (SyntaxFactor _ primary) <- factor
    verifySyntaxPrimary $ return primary

-- | Verifies a syntax primary.
-- TODO diagnostic information
verifySyntaxPrimary :: VState SyntaxPrimary -> VState RuleTag
verifySyntaxPrimary primary = do
    syntaxPrimary <- primary
    case syntaxPrimary of
        (SyntaxSpecial _) -> fail "Cannot verify special syntax."
        (TerminalProxy _) -> return Terminates
        (NonTerminalProxy nonTerminal) -> verifyNonTerminal $ return nonTerminal
        _ -> return $ 
            DoesNotTerminate [(UnableToInfer, [], "Cannot infer semantics for rule.")]

-- | Verifies a given non-terminal. 
verifyNonTerminal :: VState NonTerminal -> VState RuleTag
verifyNonTerminal nt = do
    nonTerminal <- nt
    prodMap <- gets productions
    let ntRule = M.lookup nonTerminal prodMap
    termResult <- case ntRule of
            Nothing -> checkTruthsForTermination nt
            Just (tag, body) -> do
                ntTag <- verifyRule $ return body
                modify (updateRuleTag ntTag nonTerminal)
                return ntTag
    case termResult of
        x@(DoesNotTerminate xs) -> 
            return $ DoesNotTerminate $ (addTrace nonTerminal) <$> xs
        -- (DoesNotTerminate termKind trace msg) -> 
        --     return $ DoesNotTerminate [(termKind (nonTerminal : trace) msg)]
        other -> return other
    where
        addTrace nt (termKind, trace, msg) = (termKind, (nt : trace), msg)

-- | Checks if a given non-terminal terminates in the truths block.
-- 
-- These truths are the trivial base-cases for the language semantics, and hence
-- are taken as given by the proof engine. 
checkTruthsForTermination :: VState NonTerminal -> VState RuleTag
checkTruthsForTermination nt = do
    nonTerminal <- nt
    semanticTruths <- gets truths
    if nonTerminal `elem` semanticTruths then
        return Terminates
    else
        return $ DoesNotTerminate 
            [
                (Incomplete, [], 
                "No ground truth for " ++ show nonTerminal ++ " and no\ 
                \ corresponding rule defined.")
            ]

-- | Checks if a given language rule has explicitly defined semantics.
hasSemantics :: SyntaxAlternative -> Bool
hasSemantics (SyntaxAlternative _ lrs) = case lrs of
    Just _ -> True
    Nothing -> False

-- | Combines a set of subterm termination values into a result value for the 
-- term.
combineTerminationResults 
    :: [VState RuleTag]
    -> VState RuleTag
combineTerminationResults results = do
    items <- sequence results
    return $ foldl tagPlus Terminates items
