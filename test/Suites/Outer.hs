{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Suites.Outer where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Functor.Identity      as I
import qualified Data.HashMap.Strict        as HM
import qualified Data.Vector                as V
import           System.IO.Unsafe
import           ML4HSFE.Outer
import           ML4HSFE.Loop
import           System.Exit
import           System.Process
import           Test.Tasty             (testGroup)
import           Test.Tasty.QuickCheck

tests = testGroup "Outer loop tests" [
    testProperty "Loop contains objects" arrayOfObjects
  , testProperty "Objects have clusters" objectsHaveClusters
  , testProperty "Clusters are numbers"  clustersAreNumbers
  ]

arrayOfObjects = all isOb clustered
  where isOb (Object _) = True
        isOb x          = error (show (("Expected", "Object _"),
                                       ("Got",      x)))

objectsHaveClusters = all hasCluster clustered
  where hasCluster (Object o) = "cluster" `HM.member` o

clustersAreNumbers = all clusterIsNum clustered
  where clusterIsNum (Object o) = case HM.lookup "cluster" o of
          Just (Number _) -> True
          x               -> error (show (("Expected", "Just (Number _)"),
                                          ("Got",      x)))

{-# NOINLINE clustered #-}
clustered :: [Value]
clustered = V.toList (clusterLoop (pureKmeans (Just 5))
                                  (handle width height trimmed))
  where width   = 30
        height  = 30
        trimmed = case decode rawAsts of
                    Nothing -> error "Failed to trim ASTs"
                    Just l  -> encode . take 100 $ (l :: [Value])

rawAsts :: BS.ByteString
rawAsts = unsafePerformIO
  (BS.readFile "examples/ml4hsfe-outer-loop-example-input.json")
