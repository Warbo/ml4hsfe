{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.AttoLisp       as L
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Set            as S
import qualified Data.Stringable     as Str
import qualified Data.Vector         as V
import           Generators
import           HS2AST.Sexpr
import           HS2AST.Types
import           ML4HSFE
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can extract IDs from AST"         canExtractIds
  , testProperty "Matrix rows same length"          matricesLineUp
  , testProperty "Rows merge properly"              rowsMerge
  , testProperty "Spot IDs when building matrix"    matricesHaveIds
  , testProperty "IDs get substituted"              canSubIds
  , testProperty "Can turn AST into feature matrix" canGetFeatures
  , testProperty "Can lookup JSON clusters"         canLookupClusters
  ]

canExtractIds ids = forAll (sexprWith ids) canExtract
  where canExtract expr = ids' `S.isSubsetOf` S.fromList (extractIds expr)
        ids'            = S.fromList (map cleanId ids)

mapId f (ID x y z) = ID (f x) (f y) (f z)

cleanId = mapId filterLisp

matricesLineUp :: AST -> Bool
matricesLineUp ast = length (nub (map length matrix)) == 1
  where matrix = astToMatrix ast

rowsMerge :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
rowsMerge a b c d e = mergeRows [[a, c, e], [b, d]] == expected
  where expected = trimEmpty [a ++ b, c ++ d, e]

matricesHaveIds :: Identifier -> AST -> Bool
matricesHaveIds id ast = matrixContains id matrix
  where matrix = astToMatrix (L.List [idToAst id, ast])

matrixContains :: Identifier -> PreMatrix -> Bool
matrixContains id xs = any (Just (Left (cleanId id)) `elem`) xs

idToAst id = mkNode [mkNode [mkLeaf "pkg",  mkLeaf (idPackage id)],
                     mkNode [mkLeaf "name", mkLeaf (idName    id)],
                     mkNode [mkLeaf "mod",  mkLeaf (idModule  id)]]

canSubIds = forAll (listOf (do id <- genCleanId
                               n  <- arbitrary
                               return (id, n)))
                   canSubIds'

canSubIds' :: [(Identifier, Int)] -> Property
canSubIds' ids' = forAll (genMatrixWith ids) numbersMatch
  where numbersMatch matrix = all (`elem` map snd ids') (nums matrix)
        f      = fromJust . (`lookup` ids')
        ids    = map (cleanId . fst) ids'
        nums m = [x | row <- subIdsWith f m, Just (Left x) <- row]

canSubStrings :: PreMatrix -> Bool
canSubStrings m = length (subStrings m) == length m

genMatrixWith :: [Identifier] -> Gen PreMatrix
genMatrixWith ids = fmap astToMatrix ast
  where ast = return (mkNode (map (idToAst . cleanId) ids))

genCleanId :: Gen Identifier
genCleanId = fmap cleanId arbitrary

canGetFeatures :: Property
canGetFeatures = forAll (listOf genCleanId >>= genMatrixWith) featuresGotten
  where featuresGotten m = typeCheck (getFeatures f m)
        f id = length (idName id)
        typeCheck :: Features -> Bool
        typeCheck _ = True

canLookupClusters :: [(Identifier, Feature)] -> Bool
canLookupClusters idFeatures = all found idFeatures
  where clusterString = genClusterString idFeatures
        found (id, f) = readClustered clusterString id == f

genClusterString :: [(Identifier, Feature)] -> String
genClusterString = Str.toString . encode . Array . V.fromList . map toObject
  where toObject :: (Identifier, Feature) -> Value
        toObject (id, f) = add (num f) (toJSON id)
        add :: Value -> Value -> Value
        add f (Object o) = Object (HM.insert "cluster" f o)
        num :: Feature -> Value
        num = Number . fromInteger . toInteger
