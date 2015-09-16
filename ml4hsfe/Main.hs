module Main where

import ML4HSFE
import System.Environment
import System.IO

main = do input  <- getContents
          rawAst <- getEnv "AST"
          width  <- getEnv "WIDTH"
          height <- getEnv "HEIGHT"
          putStrLn (process (read width) (read height) rawAst input)
