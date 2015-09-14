{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.AttoLisp as L
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Generators
import           HS2AST.Sexpr
import           HS2AST.Types
import           ML4HSFE
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can extract IDs from AST"      canExtractIds
  , testProperty "Matrix rows same length"       matricesLineUp
  , testProperty "Rows merge properly"           rowsMerge
  , testProperty "Spot IDs when building matrix" matricesHaveIds
  , testProperty "IDs get substituted"           canSubIds
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
