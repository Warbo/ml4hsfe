module Main where

import ML4HSFE.Loop
import System.Environment
import System.IO

main = do rawAsts <- getContents
          width   <- getEnv "WIDTH"
          height  <- getEnv "HEIGHT"
          putStrLn (handleString (read width) (read height) rawAsts)
