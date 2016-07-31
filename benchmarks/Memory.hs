module Main where

import           Common
import qualified Data.ByteString.Lazy.Char8 as BS
import           ML4HSFE.Loop
import           ML4HSFE.Outer
import           Weigh

main = mainWith $ do
  func "handleString" (handleString 10 10) (BS.unpack asts)
  io   "clusterLoop"  clusterLoop          (BS.unpack asts)
  io   "together"     together             (BS.unpack asts)

  func "handle"       (handle 10 10)                  asts
  io   "clusterLoopT" clusterLoopT      (handle 10 10 asts)
  io   "togetherT"    togetherT                       asts
