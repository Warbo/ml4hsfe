{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Suites.Outer where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import qualified Data.Vector                as V
import           System.IO.Unsafe
import           ML4HSFE.Outer
import           ML4HSFE.Loop
import qualified ML4HSFE.Types              as Ty
import           System.Exit
import           System.Process
import           Test.Tasty             (testGroup)
import           Test.Tasty.QuickCheck

tests = testGroup "Outer loop tests" [
    testProperty "Objects have clusters" objectsHaveClusters
  ]

objectsHaveClusters = all hasCluster clustered
  where hasCluster o = case Ty.entryCluster o of
                         Nothing -> False
                         Just _  -> True

{-# NOINLINE clustered #-}
clustered :: [Ty.Entry]
clustered = V.toList (clusterLoop (pureKmeans (Just 5)) asts sccs)
  where width        = 30
        height       = 30
        (asts, sccs) = handle width height trimmed
        trimmed      = case decode rawAsts of
                         Nothing -> error "Failed to trim ASTs"
                         Just l  -> encode . take 100 $ (l :: [Value])

{-# NOINLINE rawAsts #-}
rawAsts :: BS.ByteString
rawAsts = unsafePerformIO
  (BS.readFile "examples/ml4hsfe-outer-loop-example-input.json")
