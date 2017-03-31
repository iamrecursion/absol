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

import           Absol.Utilities          (countOccurrences)
import           Absol.Metaparse.Grammar
import           Absol.Metaverify.Collate
import           Absol.Metaverify.RuleTag
import           Absol.Metaverify.State
import           Data.Either              (rights)
import qualified Data.List                as L (nub)
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)

import Debug.Trace

-- TODO functions for generating nice diagnostics.
-- TODO Refactor guard checker code to have less duplication

-- | A type for storing the non-terminals defined in a syntax expression.
type NTCountMap = M.Map NonTerminal Integer

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
    combineTerminationResults result
 
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
    (SyntaxAlternative syntax semantics) <- alt
    let (LanguageRuleSemantics rule) = fromJust semantics
    let ntsInSyntax = getNTList syntax
    let ntsResult = Terminates -- TODO result from all NTs in the syntax
    semanticsResult <- case rule of
        x@EnvironmentInputRule{} -> 
            verifyEnvironmentInputRule $ return (x, ntsInSyntax)
        (EnvironmentAccessRuleProxy ear) -> 
            verifyEnvironmentAccessRule $ return (ear, ntsInSyntax)
        (SpecialSyntaxRuleProxy ssr) -> 
            verifySpecialSyntaxRule $ return (ssr, ntsInSyntax)
        (SemanticEvaluationRuleList xs) -> 
            verifySemanticEvaluationRuleList $ return (xs, ntsInSyntax)
    return $ ntsResult `tagPlus` semanticsResult

-- | Gets a list of NonTerminals and their counts from a syntactic expression.
getNTList :: [SyntaxTerm] -> NTCountMap
getNTList terms = toCountMap $ concat $ ntsInTerm <$> terms
    where
        ntsInTerm :: SyntaxTerm -> [NonTerminal]
        ntsInTerm (SyntaxTerm (SyntaxFactor _ primary) _) = ntsInPrimary primary

        ntsInPrimary :: SyntaxPrimary -> [NonTerminal]
        ntsInPrimary (SyntaxSpecial _) = []
        ntsInPrimary (TerminalProxy _) = []
        ntsInPrimary (NonTerminalProxy nt) = [nt]
        ntsInPrimary (SyntaxOptional expr) = ntsInExpr expr
        ntsInPrimary (SyntaxRepeated expr) = ntsInExpr expr
        ntsInPrimary (SyntaxGrouped expr) = ntsInExpr expr

        ntsInExpr :: SyntaxExpression -> [NonTerminal]
        ntsInExpr (SyntaxExpression alts) = concat $ ntsInAlternative <$> alts

        ntsInAlternative :: SyntaxAlternative -> [NonTerminal]
        ntsInAlternative (SyntaxAlternative altTerms _) = 
            concat $ ntsInTerm <$> altTerms

        toCountMap :: [NonTerminal] -> NTCountMap
        toCountMap nts = 
            M.fromList [ (k, countOccurrences k nts) | k <- L.nub nts ]

-- | Verifies semantics taking the form of an environment access rule.
-- 
-- TODO
verifyEnvironmentInputRule 
    :: VState (SemanticRule, NTCountMap) 
    -> VState RuleTag
verifyEnvironmentInputRule _ = return Terminates

-- | Verifies semantics taking the form of an environment input rule.
-- 
-- TODO
verifyEnvironmentAccessRule 
    :: VState (EnvironmentAccessRule, NTCountMap)
    -> VState RuleTag
verifyEnvironmentAccessRule _ = return Terminates

-- | Verifies semantics taking the form of a special syntax rule.
-- 
-- TODO
verifySpecialSyntaxRule 
    :: VState (SpecialSyntaxRule, NTCountMap) 
    -> VState RuleTag
verifySpecialSyntaxRule _ = return Terminates

-- | Verifies a list of semantic evaluation rules
verifySemanticEvaluationRuleList 
    :: VState (SemanticEvaluationRuleList, NTCountMap)
    -> VState RuleTag
verifySemanticEvaluationRuleList input = do
    args@(rules, _) <- input
    guardsComplete <- verifyGuards $ return rules
    rulesComplete <- verifySemanticRules $ return args
    let tests = [guardsComplete, rulesComplete] :: [RuleTag]
    return $ foldl tagPlus Terminates tests

-- | Verifies that the semantic rules meet their requirements.
verifySemanticRules
    :: VState (SemanticEvaluationRuleList, NTCountMap)
    -> VState RuleTag
verifySemanticRules input = do
    args@(rules, _) <- input
    satisfiesEvaluationCriterion <- verifyEvaluationCriterion $ return rules
    satisfiesSemanticForm <- verifySemanticForm $ return args
    let tests :: [RuleTag]
        tests = [satisfiesEvaluationCriterion, satisfiesSemanticForm]
    return $ foldl tagPlus Terminates tests

-- | Checks if the evaluation rules satisfy their restriction.
-- 
-- The output variable must be on the left of the leftmost evaluation rule. This
-- is the only location in which it may occur. Variables from sub-evaluations
-- may only appear on the RHS of an assignment. 
-- 
-- As it is enforced by the parser, this function can rely on having at least
-- one semantic evaluation.
verifyEvaluationCriterion 
    :: VState SemanticEvaluationRuleList
    -> VState RuleTag
verifyEvaluationCriterion _ = return Terminates

-- | Separates the variables used in the evaluations into three categories. 
-- 
-- In order, these are the output variable, any temporary that is assigned to,
-- and any variables used as part of the evaluation operations. The output
-- variable is not a temporary, and hence does not appear in the first list.
getOperationVars 
    :: [SemanticOperation] 
    -> (SemanticIdentifier, [SemanticIdentifier], [SemanticIdentifier])
getOperationVars _ = undefined

-- | Checks the semantic form of the semantic evaluation rules.
-- 
-- This checks the subterm criteria, and also the evaluation list form.
verifySemanticForm
    :: VState (SemanticEvaluationRuleList, NTCountMap)
    -> VState RuleTag
verifySemanticForm input = do
    (rules, nts) <- input
    let ntIndexPairs = getNTsFromSubEvaluations <$> rules
    let result = rights $ concat $ fmap (checkNT nts) <$> ntIndexPairs
    if null result then
        return Terminates
    else
        return $ DoesNotTerminate $ resultToErr <$> result
    where
        checkNT :: NTCountMap -> (NonTerminal, Integer) -> Either Bool String
        checkNT nts (nt, ix) = 
            if (nt `elem` M.keys nts) && (ix < M.findWithDefault 0 nt nts) then
                Left True
            else
                Right $ "Non-terminal " ++ show nt ++ " with index " ++ show ix 
                    ++ " is not defined in this production."

        resultToErr :: String -> (NonTerminationType, [NonTerminal], String)
        resultToErr str = (NonExistentSubterms, [], str)

-- | Gets the non-terminals and their indices used in the sub-evaluations.
getNTsFromSubEvaluations :: SemanticEvaluationRule -> [(NonTerminal, Integer)]
getNTsFromSubEvaluations (SemanticEvaluationRule _ _ _ _ evals) = 
    concat $ getItems <$> evals
    where
        getItems (SemanticEvaluation _ _ evalBlock) = 
            extractEval evalBlock
        extractEval (Left (SyntaxAccessBlock nt (SyntaxAccessor ix))) = 
            [(nt, ix)]
        extractEval (Right (SpecialSyntaxRule _ _ args)) = 
            concat $ extractFromArg <$> args
        extractFromArg (Left (SyntaxAccessBlock nt (SyntaxAccessor ix))) = 
            [(nt, ix)]
        extractFromArg (Right _) = [] -- These exist, as checked by the parser.
                                    
-- | Checks that the guards are complete across semantic evaluation rules.
-- 
-- It also checks that the guards only refer to variables defined as part of the
-- sub-evaluations.
verifyGuards :: VState SemanticEvaluationRuleList -> VState RuleTag
verifyGuards input = do
    guardVariablesComplete <- verifyGuardSubtermVariables input
    guardsCompleteOverDomain <- verifyGuardsComplete input
    let tests = [guardVariablesComplete, guardsCompleteOverDomain] :: [RuleTag]
    return $ foldl tagPlus Terminates tests

-- | Verifies that the patterns in the guards are complete over the domain.
-- 
-- This means that all potential values of the variables must be accounted for.
verifyGuardsComplete :: VState SemanticEvaluationRuleList -> VState RuleTag
verifyGuardsComplete input = do
    rules <- input
    let guards = extractGuards <$> rules
        -- processedGuards = normaliseGuard <$> guards
    -- traceShowM guards
    -- traceShowM processedGuards
    return Terminates

-- | Normalises a guard into a standard form. 
-- 
-- It will return an error in the case where guards aren't allowable.
-- 
-- This form is a guard with a single operator, and any constant on the right
normaliseGuard :: SemanticRestriction -> Either String [SemanticRestriction]
normaliseGuard guard = undefined

-- | Checks that the pattern guards rely only on appropriate variables.
-- 
-- Such variables should only be defined in the subterm evaluations of the 
-- semantic rules.
verifyGuardSubtermVariables
    :: VState SemanticEvaluationRuleList
    -> VState RuleTag
verifyGuardSubtermVariables input = do
    rules <- input
    let guards = extractGuards <$> rules :: [[SemanticRestriction]]
        guardVars = (L.nub . concatMap extractGuardVars) <$> guards 
    evalVars <- extractSubtermVariables $ return rules
    let groups = zip guardVars evalVars
        result = and $ checkExists <$> groups
    if result then
        return Terminates
    else
        return $ DoesNotTerminate [
                (
                    IncompleteGuards, 
                    [], 
                    "Guard refers to variables not defined in sub-evaluations." 
                )
            ]
    where
        extractGuardVars :: SemanticRestriction -> [SemanticIdentifier]
        extractGuardVars (SemVariable semId) = [semId]
        extractGuardVars (SemConstant _) = []
        extractGuardVars (SemInfixExpr _ l r) = 
            extractGuardVars l ++ extractGuardVars r

        checkExists :: (Eq a) => ([a], [a]) -> Bool
        checkExists ([], ys) = True
        checkExists (x:xs, ys) = (x `elem` ys) && checkExists (xs, ys)

-- | Extracts the guard patterns from an evaluation rule. 
extractGuards :: SemanticEvaluationRule -> [SemanticRestriction]
extractGuards 
    (SemanticEvaluationRule _ _ _ (SemanticRestrictionList guards) _) = guards

-- | Extracts the target variables from the subterm evaluations.
extractSubtermVariables 
    :: VState SemanticEvaluationRuleList
    -> VState [[SemanticIdentifier]]
extractSubtermVariables input = do
    ruleList <- input
    let evaluations = extractEvaluations <$> ruleList
    return $ fmap extractEvalVars <$> evaluations
    where
        extractEvalVars (SemanticEvaluation _ var _) = var 
        extractEvaluations (SemanticEvaluationRule _ _ _ _ evals) = evals

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
            DoesNotTerminate [
                    (UnableToInfer, [], "Cannot infer semantics for rule.")
                ]

-- | Verifies a given non-terminal. 
verifyNonTerminal :: VState NonTerminal -> VState RuleTag
verifyNonTerminal nt = do
    nonTerminal <- nt
    prodMap <- gets productions
    let ntRule = M.lookup nonTerminal prodMap
    termResult <- case ntRule of
            Nothing -> checkTruthsForTermination nt
            Just (_, body) -> do
                ntTag <- verifyRule $ return body
                modify (updateRuleTag ntTag nonTerminal)
                return ntTag
    case termResult of
        (DoesNotTerminate xs) -> 
            return $ DoesNotTerminate $ addTrace nonTerminal <$> xs
        other -> return other
    where
        addTrace nonTerm (termKind, failTrace, msg) = 
            (termKind, nonTerm : failTrace, msg)

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
