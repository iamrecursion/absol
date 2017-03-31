-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaverify.State
-- Description : State for the verification algorithm.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains the state monad and state type for the verification
-- algorithm.
--
-------------------------------------------------------------------------------
module Absol.Metaverify.State 
    (
        VState,
        ProductionMap,
        VerifierState(..),
        initVerifierState,
        defaultVerifierState,
        extractFromState,
        runState,
        evalState,
        execState,
        mapState,
        withState,
        get,
        put,
        state,
        gets,
        modify,
        updateStartRuleTag,
        updateRuleTag,
        pushProductionFrame,
        popProductionFrame
    ) where

import           Absol.Metaparse.Grammar
import           Absol.Metaverify.RuleTag
import           Control.Monad.State.Lazy
import qualified Data.Map                 as M

-- | The State monad for the verification computation.
type VState a = State VerifierState a

-- | This type is used to store the rules of the language and associated tags.
type ProductionMap = M.Map NonTerminal (RuleTag, LanguageRuleBody)

-- | This type defines the state used by the verification algorithm.
-- 
-- It tracks the rules in the language, as well as their verification state.
data VerifierState = VerifierState {
    startRule :: (RuleTag, LanguageRuleBody),
    productions :: ProductionMap,
    truths :: [NonTerminal],
    productionTrace :: [NonTerminal]
} deriving (Eq, Show)

-- | Initialises the verifier state from the AST data.
-- 
-- Eta reduction across the truths argument.
initVerifierState 
    :: StartRule 
    -> [LanguageRule] 
    -> [SemanticTruth] 
    -> VerifierState
initVerifierState (StartRule _ body) rules truths = 
    VerifierState (Untouched, body) (mapRules rules) extractTruths []
    where
        mapRules r = M.fromList [ 
                (getKey v, (Untouched, getBody v)) 
                | v <- r,  
                let getKey (LanguageRule x _) = x;
                    getBody (LanguageRule _ x) = x
            ]
        extractTruths = [ x | (SemanticTruth _ _ x) <- truths ]  

-- | Constructs a default instance of the VerifierState type.
-- 
-- This default instance should not be used for anything. 
defaultVerifierState :: VerifierState
defaultVerifierState = 
    VerifierState (Untouched, fakeStartRule) M.empty [] []
    where
        fakeStartRule = LanguageRuleBody $ SyntaxExpression []

-- | Updates the rule tag for the start rule. 
updateStartRuleTag :: RuleTag -> VerifierState -> VerifierState
updateStartRuleTag t s = do
    let (_, sRule) = startRule s
    s { startRule = (t, sRule)}

-- | Updates the tag for a given production.
updateRuleTag :: RuleTag -> NonTerminal -> VerifierState -> VerifierState
updateRuleTag tag nt st = do
    let prodMap = productions st
        foundVal = M.lookup nt prodMap
    case foundVal of
        Nothing -> 
            st { productions = M.insert nt (tag, fakeRuleBody) prodMap}
        Just (_, body) -> 
            st { productions = M.insert nt (tag, body) prodMap }
    where
        fakeRuleBody = LanguageRuleBody (SyntaxExpression [])

-- | Pulls result types out of the state monad.
extractFromState :: VState a -> a
extractFromState el = evalState el defaultVerifierState

-- | Pushes a production 'frame' onto the top of the current trace in the state.
pushProductionFrame :: NonTerminal -> VerifierState -> VerifierState
pushProductionFrame nt st@(VerifierState _ _ _ stack) = 
    st{productionTrace = nt:stack}

-- | Removes the top production 'frame' from the current trace in the state.
popProductionFrame :: VerifierState -> VerifierState
popProductionFrame st@(VerifierState _ _ _ []) = st
popProductionFrame st@(VerifierState _ _ _ (_:xs)) = st{productionTrace = xs}
