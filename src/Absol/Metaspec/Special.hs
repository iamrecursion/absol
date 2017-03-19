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
        toSpecialSyntaxName
    ) where

import           Absol.Metaparse.Grammar

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
getNonTerminals FeatureList = []
getNonTerminals FeatureMatrix = []
getNonTerminals FeatureTraverse = []
getNonTerminals FeatureFuncall = []

getSpecialSyntax :: MetaspecFeature -> [SemanticSpecialSyntax]
getSpecialSyntax FeatureBase = []
getSpecialSyntax FeatureNumber = []
getSpecialSyntax FeatureString = []
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
        SpecialSyntaxCallproc,
        SpecialSyntaxCallfun
    ]

toSpecialSyntaxName :: SemanticSpecialSyntax -> String
toSpecialSyntaxName SpecialSyntaxMap = "map"
toSpecialSyntaxName SpecialSyntaxFold = "fold"
toSpecialSyntaxName SpecialSyntaxFilter = "filter"
toSpecialSyntaxName SpecialSyntaxDefproc = "defproc"
toSpecialSyntaxName SpecialSyntaxDeffun = "deffun"
toSpecialSyntaxName SpecialSyntaxCallproc = "callproc"
toSpecialSyntaxName SpecialSyntaxCallfun = "callfun"

providesNonTerminal :: NonTerminalIdentifier -> MetaspecFeature -> Bool
providesNonTerminal nti feature = nti `elem` getNonTerminals feature

providesType :: SemanticType -> MetaspecFeature -> Bool
providesType semType feature = semType `elem` getTypes feature 

findFeatureForNT :: NonTerminalIdentifier -> (Maybe [MetaspecFeature])
findFeatureForNT nti = result (length defs)
    where
        zipped = zip availableFeatures (getNonTerminals <$>  availableFeatures)
        defs = [ x | (x, y) <- zipped, nti `elem` y]
        result len = if
            | len <= 0 -> Nothing
            | otherwise -> Just defs

findFeatureForType :: SemanticType -> (Maybe [MetaspecFeature])
findFeatureForType semType = result (length defs)
    where
        zipped = zip availableFeatures (getTypes <$>  availableFeatures)
        defs = [ x | (x, y) <- zipped, semType `elem` y]
        result len = if
            | len <= 0 -> Nothing
            | otherwise -> Just defs

findFeatureForSpecial :: SemanticSpecialSyntax -> (Maybe [MetaspecFeature])
findFeatureForSpecial syntax = result (length defs)
    where
        zipped = zip availableFeatures (getSpecialSyntax <$>  availableFeatures)
        defs = [ x | (x, y) <- zipped, syntax `elem` y]
        result len = if
            | len <= 0 -> Nothing
            | otherwise -> Just defs

availableNonTerminals :: [MetaspecFeature] -> [NonTerminalIdentifier]
availableNonTerminals features = concat $ getNonTerminals <$> features

availableTypes :: [MetaspecFeature] -> [SemanticType]
availableTypes features = concat $ getTypes <$> features

toFeatureName :: MetaspecFeature -> String
toFeatureName FeatureBase = "base"
toFeatureName FeatureNumber = "number"
toFeatureName FeatureString = "string"
toFeatureName FeatureList = "list"
toFeatureName FeatureMatrix = "matrix"
toFeatureName FeatureTraverse = "traverse"
toFeatureName FeatureFuncall = "funcall"

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

extractNTIString :: NonTerminalIdentifier -> String
extractNTIString (NonTerminalIdentifier x) = x

-- | Maps from the internal type representation to their syntactic strings.
toTypeString :: SemanticType -> String
toTypeString AnyType = "any"
toTypeString NoneType = "none"
toTypeString BoolType = "bool"
toTypeString NaturalType = "natural"
toTypeString IntegerType = "integer"
toTypeString Int32Type = "int32"
toTypeString UInt32Type = "uint32"
toTypeString Int64Type = "int64"
toTypeString UInt64Type = "uint64"
toTypeString FloatType = "float"
toTypeString DoubleType = "double"
toTypeString IntegralType = "integral"
toTypeString FloatingType = "floating"
toTypeString NumberType = "number"
toTypeString StringType = "string"
toTypeString ListType = "list"
toTypeString MatrixType = "matrix"
