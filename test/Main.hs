module Main where

import Data.List
import qualified Data.Set as S
import           Generators
import HS2AST.Sexpr
import HS2AST.Types
import           ML4HSFE
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can extract IDs from AST" canExtractIds
  , testProperty "Matrix rows same length"  matricesLineUp
  , testProperty "Rows merge properly"      rowsMerge
  ]

canExtractIds ids = forAll (sexprWith ids) canExtract
  where canExtract expr = ids' `S.isSubsetOf` S.fromList (extractIds expr)
        ids'            = S.fromList (map (mapId filterLisp) ids)

mapId f (ID x y z) = ID (f x) (f y) (f z)

matricesLineUp :: AST -> Bool
matricesLineUp ast = length (nub (map length matrix)) == 1
  where matrix = astToMatrix ast

rowsMerge :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
rowsMerge a b c d e = mergeRows [[a, c, e], [b, d]] == expected
  where expected = trimEmpty [a ++ b, c ++ d, e]
