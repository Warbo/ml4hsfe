{-# LANGUAGE OverloadedStrings #-}

module Suites.Outer where

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HM
import qualified Data.Vector                as V
import           System.IO.Unsafe
import           ML4HSFE.Outer
import           ML4HSFE.Loop
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           System.Exit
import           System.Process
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

tests = testGroup "Outer loop tests" $ if haveRunWeka
  then [
      testProperty "Loop contains objects" arrayOfObjects
    , testProperty "Objects have clusters" objectsHaveClusters
    , testProperty "Clusters are numbers"  clustersAreNumbers
    ]
  else []

arrayOfObjects = all isOb (V.toList clustered)
  where isOb (Object _) = True
        isOb x          = error (show (("Expected", "Object _"),
                                       ("Got",      x)))

objectsHaveClusters = all hasCluster (V.toList clustered)
  where hasCluster (Object o) = "cluster" `HM.member` o

clustersAreNumbers = all clusterIsNum (V.toList clustered)
  where clusterIsNum (Object o) = case HM.lookup "cluster" o of
          Just (Number _) -> True
          x               -> error (show (("Expected", "Just (Number _)"),
                                          ("Got",      x)))

{-# NOINLINE clustered #-}
clustered :: ASTs
clustered = unsafePerformIO $ do
    asts <- rawAsts
    clusterLoop (handleString width height asts)
  where width   = 30
        height  = 30

rawAsts :: IO BS.ByteString
rawAsts = BS.readFile "examples/ml4hsfe-outer-loop-example-input.json"

{-# NOINLINE haveRunWeka #-}
haveRunWeka :: Bool
haveRunWeka = unsafePerformIO $ haveCommand "runWeka"

haveCommand c = do
  (c, _, _) <- readCreateProcessWithExitCode (shell ("hash " ++ c)) ""
  return (c == ExitSuccess)
