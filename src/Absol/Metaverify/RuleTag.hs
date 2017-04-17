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
-- The type RuleTag is an instance of Monoid for practicality.
--
-------------------------------------------------------------------------------
module Absol.Metaverify.RuleTag
    (
        RuleTag(..),
        NonTerminationType(..),
        NonTerminationItem,
        tagPlus
    ) where

import           Absol.Metaparse.Grammar

type NonTerminationItem = (NonTerminationType, [NonTerminal], String)

-- | This type is used to tag each production with its verification state.
--
-- In the case where the production cannot be shown to terminate, it is tagged
-- with the kind of non-termination and the path that shows that it does not
-- terminate.
data RuleTag
    = Untouched
    | Touched
    | Terminates
    | DoesNotTerminate [NonTerminationItem]
    deriving (Eq, Show, Ord)

-- | This type is used to record the kind of non-termination that the
-- verification algorithm found.
data NonTerminationType
    = Diverges
    | Incomplete
    | IncompleteGuards
    | MalformedGuards
    | IncorrectEvaluationForm
    | NonExistentSubterms
    | UnableToInfer
    deriving (Eq, Show, Ord)

-- | Defines how to combine rule tags.
--
-- The operation is left associative, favouring the left value.
tagPlus :: RuleTag -> RuleTag -> RuleTag
tagPlus Untouched _ = Untouched
tagPlus _ Untouched = Untouched
tagPlus (DoesNotTerminate xs) (DoesNotTerminate ys) =
    DoesNotTerminate $ xs ++ ys
tagPlus x@DoesNotTerminate{} _ = x
tagPlus _ x@DoesNotTerminate{} = x
tagPlus Terminates Terminates = Terminates
tagPlus Touched x = x
tagPlus x Touched = x

-- | The Monoid instance for RuleTag.
instance Monoid RuleTag where
    mempty = Untouched
    mappend = tagPlus
    mconcat = foldr mappend mempty
