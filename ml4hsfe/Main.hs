module Main where

import ML4HSFE
import System.Environment
import System.IO

main = do rawAst <- getContents
          width  <- getEnv "WIDTH"
          height <- getEnv "HEIGHT"
          putStrLn (process (read width) (read height) rawAst)
