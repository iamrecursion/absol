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
        updateStartRuleTag
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
    truths :: [SemanticTruth]
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
    VerifierState (Untouched, body) (mapRules rules) truths
    where
        mapRules r = M.fromList [ 
                (getKey v, (Untouched, getBody v)) 
                | v <- r,  
                let getKey (LanguageRule x _) = x;
                    getBody (LanguageRule _ x) = x
            ]

-- | Updates the rule tag for the start rule. 
updateStartRuleTag :: RuleTag -> VerifierState -> VerifierState
updateStartRuleTag t s = do
    let (_, sRule) = startRule s
    s { startRule = (t, sRule)}
