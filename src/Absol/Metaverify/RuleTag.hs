-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaverify.RuleTag
-- Description : Tag types to help the verification process.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- Contains a type used for tagging each production with its verification state.
--
-------------------------------------------------------------------------------
module Absol.Metaverify.RuleTag
    (
        RuleTag(..),
        NonTerminationType(..),
        tagPlus
    ) where

import           Absol.Metaparse.Grammar

-- | This type is used to tag each production with its verification state.
-- 
-- In the case where the production cannot be shown to terminate, it is tagged
-- with the kind of non-termination and the path that shows that it does not
-- terminate.
data RuleTag
    = Untouched
    | Terminates
    | DoesNotTerminate NonTerminationType [NonTerminal] String
    deriving (Eq, Show, Ord)

-- | This type is used to record the kind of non-termination that the
-- verification algorithm found.
data NonTerminationType
    = Diverges
    | Incomplete
    deriving (Eq, Show, Ord)

-- | Defines how to combine rule tags.
-- 
-- The operation is left associative, favouring the left value.
tagPlus :: RuleTag -> RuleTag -> RuleTag
tagPlus Untouched _ = Untouched
tagPlus _ Untouched = Untouched
tagPlus Terminates Terminates = Terminates
tagPlus x@DoesNotTerminate{} _ = x
tagPlus _ x@DoesNotTerminate{} = x
