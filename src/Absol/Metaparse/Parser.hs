-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaparse.Parser
-- Description : Provides a stateful parser type for parsing metaspec.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides a Parser with backtracking user-accessible state.
--
-------------------------------------------------------------------------------
module Absol.Metaparse.Parser
    (
        ParserST,
        MetaState(..),
        initParserState,
        runStateT,
        updateImportedFeatures,
        addNTIdentifier,
        setPositionHead,
        setPositionBody,
        setPositionNone,
        checkNTsInLang,
        checkNTNotDefined,
        checkTypeDefined,
        checkSpecialSyntaxAvailable,
        modify,
        get,
        put,
        state
    ) where

import           Absol.Metaparse.Grammar
import           Absol.Metaspec.Special
import           Control.Monad.State.Lazy
import           Data.List                (foldl', intercalate)
import qualified Data.Set                 as S
import           Text.Megaparsec.Text     (Parser)

-- | Provides a parser type with backtracking user state.
type ParserST = StateT MetaState Parser

-- | Tracks whether the parser is on the left or right of a defining symbol.
data RulePosition = Head | Body | None deriving (Show)

-- | The parser state.
data MetaState = MetaState {
    importedFeatures      :: [MetaspecFeature],
    definedNTs            :: [NonTerminalIdentifier],
    usedNTs               :: S.Set NonTerminalIdentifier,
    importedTypes         :: [SemanticType],
    importedSpecialSyntax :: [SemanticSpecialSyntax],
    parserPosition        :: RulePosition
} deriving (Show)

-- | Generates an initial state for the parser.
initParserState :: MetaState
initParserState = MetaState [] [] S.empty [] [] None

-- | Updates the list of imported features in the parser state.
updateImportedFeatures :: [MetaspecFeature] -> MetaState -> MetaState
updateImportedFeatures list st = st {
        importedFeatures = list,
        definedNTs = concat $ getNonTerminals <$> list,
        importedTypes = concat $ getTypes <$> list,
        importedSpecialSyntax = concat $ getSpecialSyntax <$> list
    }

-- | Updates the list of defined non-terminals in the parser state.
updateDefinedNTs :: NonTerminalIdentifier -> MetaState -> MetaState
updateDefinedNTs item st = st {definedNTs = definedNTs st ++ [item]}

-- | Updates the list of used non-terminals in the parser state.
updateUsedNTs :: NonTerminalIdentifier -> MetaState -> MetaState
updateUsedNTs identifier st = st {usedNTs = S.insert identifier (usedNTs st) }

-- | Adds a non-terminal identifier to the state based on parser position.
--
-- The behaviour of this function depends on the parser position in a language
-- production, as indicated by the state.
addNTIdentifier :: NonTerminalIdentifier -> MetaState -> MetaState
addNTIdentifier ident x@MetaState{parserPosition = Head} =
    updateDefinedNTs ident x
addNTIdentifier ident x@MetaState{parserPosition = Body} =
    updateUsedNTs ident x
addNTIdentifier _ x@MetaState{parserPosition = None} = x

-- | Sets the current parser position to be in the head of a production.
setPositionHead :: MetaState -> MetaState
setPositionHead st = st {parserPosition = Head}

-- | Sets the current parser position to be in the body of a production.
setPositionBody :: MetaState -> MetaState
setPositionBody st = st {parserPosition = Body}

-- | Sets the current parser position to not be in a language rule.
setPositionNone :: MetaState -> MetaState
setPositionNone st = st {parserPosition = None}

-- | Checks whether the used identifiers have been defined in the language.
--
-- If all identifiers have been defined, it returns the value True. If there are
-- any used identifiers that have not been defined, it returns a list containing
-- these undefined identifiers.
checkNTsInLang :: ParserST (Either [NonTerminalIdentifier] Bool)
checkNTsInLang = do
    definedNTVals <- gets definedNTs
    usedNTVals <- gets usedNTs
    let bools = (`elem` definedNTVals) <$> S.toList usedNTVals
        result = foldl' (&&) True bools
    if result then
        return (Right $ foldl' (&&) True bools)
    else
        return (Left $ failures (S.toList usedNTVals) bools)
    where
        failures nts bools = [ y | (x,y) <- zip bools nts, not x ]

-- | Checks whether the NT has already been defined.
--
-- If the parsed NT has been defined before in this file, it will error.
checkNTNotDefined
    :: ParserST NonTerminalIdentifier
    -> ParserST NonTerminalIdentifier
checkNTNotDefined ident = do
    unpackedId <- ident
    definedIdentifiers <- gets definedNTs
    parsePos <- gets parserPosition

    -- Check definition of the current NT.
    case (unpackedId `elem` definedIdentifiers, parsePos) of
        (_, None)     -> return unpackedId
        (False, Head) -> return unpackedId
        (True, Head)  -> failExpr unpackedId
        (_, Body)     -> return unpackedId
    where
        failExpr nt@(NonTerminalIdentifier x) =
            fail $ "Non-Terminal with name \"" ++ x ++ "\" already defined. "
                ++ suggest nt
        suggest x = case findFeatureForNT x of
            Nothing -> "Defined elsewhere in document."
            Just xs -> "Defined by language feature(s): " ++ feats xs ++ "."
            where
                feats y = intercalate ", " $ toFeatureName <$> y

-- | Checks if a given type is defined in the current language context.
--
-- If the type doesn't exist in scope, it will determine which feature needs to
-- be imported if the type exists, otherwise error.
checkTypeDefined :: ParserST SemanticType -> ParserST SemanticType
checkTypeDefined t = do
    semType <- t
    scopeTypes <- gets importedTypes
    if semType `elem` scopeTypes then return semType else
        failExpr semType
        where
            failExpr x = fail $ failStr x
            failStr x = "Type \"" ++ toTypeString x ++ "\" not in scope. "
                ++ suggest x
            suggest x = case findFeatureForType x of
                Nothing -> "Does not exist."
                Just f -> "Defined in language feature(s): " ++ feats f ++ "."
            feats x = intercalate ", " $ toFeatureName <$> x

-- | Checks if a given piece of special syntax is in scope.
--
-- If the syntax is not in scope it will suggest the import for the used syntax.
checkSpecialSyntaxAvailable
    :: ParserST SemanticSpecialSyntax
    -> ParserST SemanticSpecialSyntax
checkSpecialSyntaxAvailable parser = do
    specialSyntax <- parser
    scopeSyntax <- gets importedSpecialSyntax
    if specialSyntax `elem` scopeSyntax then return specialSyntax else
        failExpr specialSyntax
    where
        failExpr x = fail $ failStr x
        failStr x = "Special Syntax \"" ++ toSpecialSyntaxName x
            ++ "\" not in scope. " ++ suggest x
        suggest x = case findFeatureForSpecial x of
            Nothing -> "Feature does not exist."
            Just f -> "Please import one of the following: " ++ feats f ++ "."
        feats x = intercalate ", " $ toFeatureName <$> x
