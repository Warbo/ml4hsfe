module Main where

import           Generators
import           ML4HSFE
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Can extract IDs from AST" canExtractIds
  ]

canExtractIds ids = forAll (sexprWith ids) canExtract
  where canExtract expr = extractIds expr == ids
