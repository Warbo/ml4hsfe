{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Main where

import           Common
import           Control.DeepSeq
import           Criterion.Main
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           ML4HSFE.Loop
import           ML4HSFE.Outer

-- Our benchmark harness.
main = defaultMain [
      bgroup "Stringy" [
          bench "handleString" (nf   (handleString 10 10) (BS.unpack asts))
        , bench "clusterLoop"  (nfIO (clusterLoop         (BS.unpack asts)))
        , bench "together"     (nfIO (together            (BS.unpack asts)))
        ]
    , bgroup "Texty" [
          bench "handle"       (nf   (handle       10 10)            asts)
        , bench "clusterLoopT" (nfIO (clusterLoopT     (handle 10 10 asts)))
        , bench "togetherT"    (nfIO (togetherT                      asts))
        ]
  ]

-- Required instances

--instance NFData ASTs
