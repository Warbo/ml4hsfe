module Main where

import qualified Suites.ML4HSFE as FE
import qualified Suites.Order   as Order
import qualified Suites.Outer   as Outer
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    FE.tests
  , Order.tests
  , Outer.tests
  ]
