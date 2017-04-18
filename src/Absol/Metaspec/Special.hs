-------------------------------------------------------------------------------
-- |
-- Module      : Absol.Metaspec.Special
-- Description : Support functions for the metaspec language features.
-- Copyright   : (c) Ara Adkins (2017)
-- License     : See LICENSE file
--
-- Maintainer  : Ara Adkins
-- Stability   : experimental
-- Portability : GHC
--
-- A module providing metacompiler-level support for the language special
-- features.
--
-------------------------------------------------------------------------------
module Absol.Metaspec.Special
    (
        getTypes,
        getNonTerminals,
        getSpecialSyntax,
        providesNonTerminal,
        providesType,
        toFeatureName,
        findFeatureForNT,
        findFeatureForType,
        findFeatureForSpecial,
        extractNTIString,
        toTypeString,
        availableNonTerminals,
        availableTypes,
        toSpecialSyntaxName
    ) where

import           Absol.Metaparse.Grammar

-- | Gets the types defined by a particular feature.
getTypes :: MetaspecFeature -> [SemanticType]
getTypes FeatureBase = [AnyType, NoneType, BoolType]
getTypes FeatureNumber =
    [
        NaturalType,
        IntegerType,
        Int32Type,
        UInt32Type,
        Int64Type,
        UInt64Type,
        FloatType,
        DoubleType,
        IntegralType,
        FloatingType,
        NumberType
    ]
getTypes FeatureString = [StringType]
getTypes FeatureList = [ListType]
getTypes FeatureMatrix = [MatrixType]
getTypes FeatureTraverse = []
getTypes FeatureFuncall = []

-- | Gets the non-terminals defined by a particular feature.
getNonTerminals :: MetaspecFeature -> [NonTerminalIdentifier]
getNonTerminals FeatureBase = makeNTI <$>
    [
        "digit",
        "nondigit",
        "utf-8-char",
        "bool",
        "space-char",
        "newline"
    ]
getNonTerminals FeatureNumber = makeNTI <$>
    [
        "natural",
        "integer",
        "int32",
        "int64",
        "uint32",
        "uint64",
        "float",
        "double",
        "integral",
        "floating",
        "number"
    ]
getNonTerminals FeatureString = makeNTI <$> ["string"]
getNonTerminals FeatureList = makeNTI <$> ["list"]
getNonTerminals FeatureMatrix = makeNTI <$> ["matrix"]
getNonTerminals FeatureTraverse = []
getNonTerminals FeatureFuncall = []

-- | Gets the special syntax elements defined by features.
getSpecialSyntax :: MetaspecFeature -> [SemanticSpecialSyntax]
getSpecialSyntax FeatureBase = 
    [
        SpecialSyntaxEnvStore,
        SpecialSyntaxEnvGet,
        SpecialSyntaxEnvGetDefault,
        SpecialSyntaxNodeLength,
        SpecialSyntaxSemanticsOf
    ]
getSpecialSyntax FeatureNumber = 
    [
        SpecialSyntaxCiel,
        SpecialSyntaxFloor
    ]
getSpecialSyntax FeatureString = 
    [
        SpecialSyntaxRev,
        SpecialSyntaxSplit,
        SpecialSyntaxJoin
    ]
getSpecialSyntax FeatureList = []
getSpecialSyntax FeatureMatrix = []
getSpecialSyntax FeatureTraverse =
    [
        SpecialSyntaxMap,
        SpecialSyntaxFold,
        SpecialSyntaxFilter
    ]
getSpecialSyntax FeatureFuncall =
    [
        SpecialSyntaxDefproc,
        SpecialSyntaxDeffun,
        SpecialSyntaxCallfun
    ]

-- | Converts a special syntax element to their metaspec form.
toSpecialSyntaxName :: SemanticSpecialSyntax -> String
toSpecialSyntaxName SpecialSyntaxMap      = "map"
toSpecialSyntaxName SpecialSyntaxFold     = "fold"
toSpecialSyntaxName SpecialSyntaxFilter   = "filter"
toSpecialSyntaxName SpecialSyntaxDeffun   = "deffun"
toSpecialSyntaxName SpecialSyntaxDefproc = "callproc"
toSpecialSyntaxName SpecialSyntaxCallfun  = "callfun"
toSpecialSyntaxName SpecialSyntaxEnvStore = "envStore"
toSpecialSyntaxName SpecialSyntaxEnvGet = "envGet"
toSpecialSyntaxName SpecialSyntaxEnvGetDefault = "envGetDefault"
toSpecialSyntaxName SpecialSyntaxNodeLength = "nodeLength"
toSpecialSyntaxName SpecialSyntaxCiel = "ciel"
toSpecialSyntaxName SpecialSyntaxFloor = "floor"
toSpecialSyntaxName SpecialSyntaxRev = "rev"
toSpecialSyntaxName SpecialSyntaxSplit = "split"
toSpecialSyntaxName SpecialSyntaxJoin = "join"
toSpecialSyntaxName SpecialSyntaxSemanticsOf = "semanticsOf"

-- | Checks if a given feature provides a non-terminal.
providesNonTerminal :: NonTerminalIdentifier -> MetaspecFeature -> Bool
providesNonTerminal nti feature = nti `elem` getNonTerminals feature

-- | Checks if a given feature provides a type.
providesType :: SemanticType -> MetaspecFeature -> Bool
providesType semType feature = semType `elem` getTypes feature

-- | Finds the feature corresponding to a non-terminal.
findFeatureForNT :: NonTerminalIdentifier -> Maybe [MetaspecFeature]
findFeatureForNT nti = findFeatureForX nti getNonTerminals

-- | Finds the feature corresponding to a type.
findFeatureForType :: SemanticType -> Maybe [MetaspecFeature]
findFeatureForType semType = findFeatureForX semType getTypes

-- | Finds the feature corresponding to a special-syntax element.
findFeatureForSpecial :: SemanticSpecialSyntax -> Maybe [MetaspecFeature]
findFeatureForSpecial syntax = findFeatureForX syntax getSpecialSyntax

-- | Finds the feature corresponding to a given input (using an accessor fn).
findFeatureForX
    :: (Eq a)
    => a
    -> (MetaspecFeature -> [a])
    -> Maybe [MetaspecFeature]
findFeatureForX item fn = result (length defs)
    where
        zipped = zip availableFeatures (fn <$> availableFeatures)
        defs = [ x | (x, y) <- zipped, item `elem` y]
        result len = if
            | len <= 0 -> Nothing
            | otherwise -> Just defs

-- | Gets a list of the non-terminals made available by a set of features.
availableNonTerminals :: [MetaspecFeature] -> [NonTerminalIdentifier]
availableNonTerminals features = concat $ getNonTerminals <$> features

-- | Gets a list of the types made available by a set of features.
availableTypes :: [MetaspecFeature] -> [SemanticType]
availableTypes features = concat $ getTypes <$> features

-- | Translates feature instances to their import names.
toFeatureName :: MetaspecFeature -> String
toFeatureName FeatureBase     = "base"
toFeatureName FeatureNumber   = "number"
toFeatureName FeatureString   = "string"
toFeatureName FeatureList     = "list"
toFeatureName FeatureMatrix   = "matrix"
toFeatureName FeatureTraverse = "traverse"
toFeatureName FeatureFuncall  = "funcall"

-- | The list of features available in the language.
availableFeatures :: [MetaspecFeature]
availableFeatures =
    [
        FeatureBase,
        FeatureNumber,
        FeatureString,
        FeatureList,
        FeatureMatrix,
        FeatureTraverse,
        FeatureFuncall
    ]

-- | Constructs a non-terminal identifier from a string.
makeNTI :: String -> NonTerminalIdentifier
makeNTI = NonTerminalIdentifier

-- | Extracts the string from the Non-Terminal.
extractNTIString :: NonTerminalIdentifier -> String
extractNTIString (NonTerminalIdentifier x) = x

-- | Maps from the internal type representation to their syntactic strings.
toTypeString :: SemanticType -> String
toTypeString AnyType      = "any"
toTypeString NoneType     = "none"
toTypeString BoolType     = "bool"
toTypeString NaturalType  = "natural"
toTypeString IntegerType  = "integer"
toTypeString Int32Type    = "int32"
toTypeString UInt32Type   = "uint32"
toTypeString Int64Type    = "int64"
toTypeString UInt64Type   = "uint64"
toTypeString FloatType    = "float"
toTypeString DoubleType   = "double"
toTypeString IntegralType = "integral"
toTypeString FloatingType = "floating"
toTypeString NumberType   = "number"
toTypeString StringType   = "string"
toTypeString ListType     = "list"
toTypeString MatrixType   = "matrix"
