{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Main where

import qualified CoreSyn             as C
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import qualified Data.Set            as S
import qualified Data.Vector         as V
import           FastString
import           ML4HSFE.FeatureExtraction
import           ML4HSFE.Generators
import           ML4HSFE.Types
import           PackageConfig
import qualified Suites.ML4HSFE as FE
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    FE.tests
  ]
