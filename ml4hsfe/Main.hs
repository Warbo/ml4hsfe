module Main where

import ML4HSFE
import System.Environment
import System.IO

main = do input  <- getContents
          rawAst <- getEnv "AST"
          putStrLn (process rawAst input)
