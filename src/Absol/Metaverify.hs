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
import           Absol.Metaverify.Diagnostics
import           Absol.Metaverify.RuleTag
import           Absol.Metaverify.State
import           Absol.Utilities              (countOccurrences)
import           Data.Either                  (lefts, rights)
import qualified Data.List                    as L (delete, find, nub,
                                                    permutations)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust, isJust)
import qualified Data.Set                     as S

import           Debug.Trace

-- | A type for storing the non-terminals defined in a syntax expression.
type NTCountMap = M.Map NonTerminal Integer

-- | Verifies the input language.
--
-- If the language can be proven complete, this returns True. In the case where
-- that property cannot be shown, an error string is returned, describing the
-- nature of the error.
--
-- It will also alert the user to any unused productions.
verifyLanguage :: Metaspec -> (Bool, String)
verifyLanguage x = case runState runVerification (collateASTData x) of
    (True, VerifierState (tag, _) _ _ _) ->
        (True, "LANGUAGE: " ++ prettyPrintRuleTag tag)
    (False, VerifierState (tag, _) prod _ _) ->
        (False,
            "LANGUAGE: " ++ prettyPrintRuleTag tag ++ "\n\n" ++
            printLanguageDiagnostics prod
        )

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
        _          -> return False

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
    sequence_ $ (markAsTouched . return) <$> M.keys ntsInSyntax
    case rule of
        x@EnvironmentInputRule{} ->
            verifyEnvironmentInputRule $ return (x, ntsInSyntax)
        (EnvironmentAccessRuleProxy ear) ->
            verifyEnvironmentAccessRule $ return (ear, ntsInSyntax)
        (SpecialSyntaxRuleProxy ssr) ->
            verifySpecialSyntaxRule $ return (ssr, ntsInSyntax)
        (SemanticEvaluationRuleList xs) ->
            verifySemanticEvaluationRuleList $ return (xs, ntsInSyntax)

-- Marks a given non-terminal as having been visited but not processed.
markAsTouched :: VState NonTerminal -> VState ()
markAsTouched nt = do
    nonTerminal <- nt
    modify (updateRuleTag Touched nonTerminal)
    return ()

-- | Gets a list of NonTerminals and their counts from a syntactic expression.
getNTList :: [SyntaxTerm] -> NTCountMap
getNTList terms = toCountMap $ concat $ ntsInTerm <$> terms
    where
        ntsInTerm :: SyntaxTerm -> [NonTerminal]
        ntsInTerm (SyntaxTerm (SyntaxFactor _ primary) _) = ntsInPrimary primary

        ntsInPrimary :: SyntaxPrimary -> [NonTerminal]
        ntsInPrimary (SyntaxSpecial _)     = []
        ntsInPrimary (TerminalProxy _)     = []
        ntsInPrimary (NonTerminalProxy nt) = [nt]
        ntsInPrimary (SyntaxOptional expr) = ntsInExpr expr
        ntsInPrimary (SyntaxRepeated expr) = ntsInExpr expr
        ntsInPrimary (SyntaxGrouped expr)  = ntsInExpr expr

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
-- Environment stores will always terminate, so this function needs to verify if
-- the operands exist and that they themselves terminate.
verifyEnvironmentInputRule
    :: VState (SemanticRule, NTCountMap)
    -> VState RuleTag
verifyEnvironmentInputRule input = do
    (rule, nts) <- input
    case rule of
        (EnvironmentInputRule _ accessBlock accessList) -> do
            let accessBlocks = accessBlock:accessList
                ntPairs = extractSyntaxAccessBlock <$> accessBlocks
                result = rights $ checkNT nts <$> ntPairs
            subResults <- sequence
                $ (\(x,_) -> verifyNonTerminal $ return x) <$> ntPairs
            let subResult = foldl tagPlus Terminates subResults
            if null result then
                return $ Terminates `tagPlus` subResult
            else
                return $ (DoesNotTerminate $ (resultToErr NonExistentSubterms) <$> result)
                    `tagPlus` subResult
        _ -> return Terminates

-- | Extracts syntax access blocks into a suitable form for processing.
extractSyntaxAccessBlock :: SyntaxAccessBlock -> (NonTerminal, Integer)
extractSyntaxAccessBlock (SyntaxAccessBlock nt (SyntaxAccessor i)) = (nt, i)

-- | Verifies semantics taking the form of an environment input rule.
--
-- Access rules depend only on the thing having been stored, and have well-
-- defined semantics in either case. They have already been verified to rely
-- on terminals that exist, and so the checking is not performed.
--
-- Environment accesses have a well-defined error state in cases where the
-- element does not exist.
verifyEnvironmentAccessRule
    :: VState (EnvironmentAccessRule, NTCountMap)
    -> VState RuleTag
verifyEnvironmentAccessRule _ = return Terminates

-- | Verifies semantics taking the form of a special syntax rule.
--
-- Special Syntax Rules themselves are guaranteed to terminate, so this function
-- just needs to check if the operands exist, and that they themselves will
-- terminate.
verifySpecialSyntaxRule
    :: VState (SpecialSyntaxRule, NTCountMap)
    -> VState RuleTag
verifySpecialSyntaxRule input = do
    (SpecialSyntaxRule _ _ accessList, nts) <- input
    let ntList = fromJust <$>
            filter isJust (getNTsFromAccessBlockOrRule <$> accessList)
        result = rights $ checkNT nts <$> ntList
    subResults <- sequence $ (\(x,_) -> verifyNonTerminal $ return x) <$> ntList
    let subResult = foldl tagPlus Terminates subResults
    if null result then
        return $ Terminates `tagPlus` subResult
    else
        return $ (DoesNotTerminate $
            resultToErr NonExistentSubterms <$> result) `tagPlus` subResult

-- | Gets the non-terminals from syntax access blocks or env access rules.
getNTsFromAccessBlockOrRule :: AccessBlockOrRule -> Maybe (NonTerminal, Integer)
getNTsFromAccessBlockOrRule (Left (SyntaxAccessBlock nt (SyntaxAccessor i))) =
    Just (nt, i)
getNTsFromAccessBlockOrRule (Right _) = Nothing

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
verifyEvaluationCriterion list = do
    rules <- list
    let rulePairs = getOutputRulePair <$> rules
        opVars = getOperationVars <$> rulePairs
        evalVars = getEvaluationVars <$> rules
        results = checkVariableEvalCriteria <$> zip opVars evalVars
        allVarsInOrder = concat $ (getVarsInOrderForEvals . snd) <$> rulePairs 
        varsOrdered = and $ checkVarDefinitionOrdering <$> allVarsInOrder
    if (and results) && varsOrdered then
        return Terminates
    else
        return $ DoesNotTerminate
            [(IncorrectEvaluationForm, [], "Malformed semantic operation(s).")]
    where
        getOutputRulePair
            :: SemanticEvaluationRule
            -> (SemanticIdentifier, [SemanticOperationAssignment])
        getOutputRulePair
            (SemanticEvaluationRule _ ident (SemanticOperationList evals) _ _) =
            (ident, evals)

-- | Gets the list of variables defined before and after a temporary.
-- 
-- The output is [before], temporary, [after].
getVarsInOrderForEvals 
    :: [SemanticOperationAssignment] 
    -> [(SemanticIdentifier, [SemanticIdentifier])]
getVarsInOrderForEvals [] = []
getVarsInOrderForEvals ((SemanticOperationAssignment x op):xs) = 
    (x, getEvalVar op ++ (concat $ evalVars <$> xs)) : getVarsInOrderForEvals xs

-- | Ensures that for input [before], temporary, [after] that temporary is not
-- a member of after.
-- 
-- This ensures the final criteria to verify the semantic evaluations.
checkVarDefinitionOrdering
    :: (SemanticIdentifier, [SemanticIdentifier])
    -> Bool
checkVarDefinitionOrdering (semId, ids) = semId `notElem` ids

-- | Checks that the usage of variables in the semantic operation is correct.
--
-- This means that it has to obey the evaluation rules.
checkVariableEvalCriteria
    :: (
            (SemanticIdentifier, [SemanticIdentifier], [SemanticIdentifier]),
            [SemanticIdentifier]
        )
    -> Bool
checkVariableEvalCriteria ((output, temps, vars), evalVars) = let
        evalsNotInTemps = and $ (`notElem` temps) <$> evalVars
        varsInTempOrEval = and $ (`elem` (temps ++ evalVars)) <$> vars
        outNotInVars = output `notElem` vars
        outNotInTemps = output `notElem` temps
        outNotInEval = output `notElem` evalVars
    in
        evalsNotInTemps && outNotInVars && outNotInTemps
            && outNotInEval && varsInTempOrEval

-- | Separates the variables used in the evaluations into three categories.
--
-- In order, these are the output variable, any temporary that is assigned to,
-- and any variables used as part of the evaluation operations. The output
-- variable is not a temporary, and hence does not appear in the first list.
getOperationVars
    :: (SemanticIdentifier, [SemanticOperationAssignment])
    -> (SemanticIdentifier, [SemanticIdentifier], [SemanticIdentifier])
getOperationVars (ident, opAssigns) =
    (ident, L.delete ident (temps <$> opAssigns),
        concat (evalVars <$> opAssigns)
    )
    where
        temps :: SemanticOperationAssignment -> SemanticIdentifier
        temps (SemanticOperationAssignment identifier _) = identifier

-- | Extracts the variables on the RHS of a semantic evaluation assignment.
evalVars :: SemanticOperationAssignment -> [SemanticIdentifier]
evalVars (SemanticOperationAssignment _ op) = getEvalVar op

-- | Extracts the variables on the RHS of a semantic evaluation.
getEvalVar :: SemanticOperation -> [SemanticIdentifier]
getEvalVar (Variable identifier) = [identifier]
getEvalVar (VariableAccess identifier _) = [identifier]
getEvalVar (Constant _) = []
getEvalVar (Parentheses op) = getEvalVar op
getEvalVar (PrefixExpr _ op) = getEvalVar op
getEvalVar (PostfixExpr _ op) = getEvalVar op
getEvalVar (InfixExpr _ op1 op2) = getEvalVar op1 ++ getEvalVar op2

-- | Gets the variables defined by the sub-evaluations.
getEvaluationVars :: SemanticEvaluationRule -> [SemanticIdentifier]
getEvaluationVars (SemanticEvaluationRule _ _ _ _ evals) = getVar <$> evals
    where
        getVar (SemanticEvaluation _ ident _) = ident

-- | Checks the semantic form of the semantic evaluation rules.
--
-- This checks the subterm criteria, and also the evaluation list form.
verifySemanticForm
    :: VState (SemanticEvaluationRuleList, NTCountMap)
    -> VState RuleTag
verifySemanticForm input = do
    (rules, nts) <- input
    let ntIndexPairs = getNTsFromSubEvaluations <$> rules
    _ <- sequence $ (\(x,_) -> (verifyNonTerminal . return) x) <$>
        concat ntIndexPairs
    let result = rights $ concat $ fmap (checkNT nts) <$> ntIndexPairs
    if null result then
        return Terminates
    else
        return $ DoesNotTerminate $ (resultToErr NonExistentSubterms) <$> result

-- | Converts an error string into a non-termination result with given type.
resultToErr
    :: NonTerminationType
    -> String
    -> (NonTerminationType, [NonTerminal], String)
resultToErr ntType str = (ntType, [], str)

-- | Checks if a non-terminal is defined in the syntax properly.
checkNT :: NTCountMap -> (NonTerminal, Integer) -> Either Bool String
checkNT nts (nt, ix) =
    if (nt `elem` M.keys nts) && (ix < M.findWithDefault 0 nt nts) then
        Left True
    else
        Right $ "Non-terminal " ++ show nt ++ " with index " ++ show ix
            ++ " is not defined in this production."

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
-- TODO Complete guard verification.
verifyGuards :: VState SemanticEvaluationRuleList -> VState RuleTag
verifyGuards input = do
    guardVariablesComplete <- verifyGuardSubtermVariables input
    guardsCompleteOverDomain <- verifyGuardsComplete input
    let tests = [guardVariablesComplete, guardsCompleteOverDomain] :: [RuleTag]
    return $ foldl tagPlus Terminates tests

-- | Verifies that the patterns in the guards are complete over the domain.
--
-- As this problem is actually
verifyGuardsComplete :: VState SemanticEvaluationRuleList -> VState RuleTag
verifyGuardsComplete input = do
    rules <- input
    let guards = extractGuards <$> rules
        containsCatchallGuard = L.find null guards
    case containsCatchallGuard of
        Nothing -> return $ DoesNotTerminate [
                (
                    IncompleteGuards,
                    [],
                    "Guards must contain a catch-all clause.")
            ]
        Just _ -> return Terminates

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
        checkExists ([], _)    = True
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
-- 
-- In general, the inference is restricted to a single instance of a terminal
-- or non-terminal symbol. 
verifySubSemantics :: VState SyntaxAlternative -> VState RuleTag
verifySubSemantics alt = do
    (SyntaxAlternative terms _) <- alt
    if length terms > 1 then
        return $ DoesNotTerminate [
                (UnableToInfer, [], "Cannot infer semantics for rule.")
            ]
    else do
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
    (SyntaxFactor repeat primary) <- factor
    case repeat of
        (Just _) -> return $ DoesNotTerminate [
                (UnableToInfer, 
                    [], "Cannot infer semantics for rule with repetition.")
            ]
        Nothing -> verifySyntaxPrimary $ return primary

-- | Verifies a syntax primary.
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
--
-- Uses the 'Touched' value constructor and stack value checks to ensure that
-- any mutually-recursive productions can be verified correctly.
verifyNonTerminal :: VState NonTerminal -> VState RuleTag
verifyNonTerminal nt = do
    nonTerminal <- nt
    prodMap <- gets productions
    let ntRule = M.lookup nonTerminal prodMap

    -- Production stack frame
    modify (pushProductionFrame nonTerminal)
    prodTrace <- gets productionTrace
    termResult <- case ntRule of
        Nothing -> checkTruthsForTermination nt
        Just (tag, body) -> do
            case tag of
                Terminates -> return tag
                (DoesNotTerminate _) -> return tag
                -- Only process if there is no termination tag assigned
                _ -> do
                    modify (updateRuleTag Touched nonTerminal)
                    if nonTerminal `elem` tail prodTrace then
                        return tag
                    else do
                        ntTag <- verifyRule $ return body
                        modify (updateRuleTag ntTag nonTerminal)
                        return ntTag
    modify popProductionFrame
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
    Just _  -> True
    Nothing -> False

-- | Combines a set of subterm termination values into a result value for the
-- term.
combineTerminationResults
    :: [VState RuleTag]
    -> VState RuleTag
combineTerminationResults results = do
    items <- sequence results
    return $ foldl tagPlus Terminates items
