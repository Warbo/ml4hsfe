module Main where

import ML4HSFE.Loop
import ML4HSFE.Outer
import System.Environment
import System.IO

main = do rawAsts <- getContents
          width   <- getEnv "WIDTH"
          height  <- getEnv "HEIGHT"
          asts    <- clusterLoop (handleString (read width) (read height) rawAsts)
          putStrLn (renderAsts asts)
