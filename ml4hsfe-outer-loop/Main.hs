module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import           ML4HSFE.Loop
import           ML4HSFE.Outer
import           System.Environment

main = do rawAsts <- BS.getContents
          width   <- getEnv "WIDTH"
          height  <- getEnv "HEIGHT"
          asts    <- clusterLoop (handleString (read width) (read height) rawAsts)
          putStrLn (renderAsts asts)
