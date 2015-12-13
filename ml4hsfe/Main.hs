module Main where

import ML4HSFE
import System.Environment
import System.IO

main = do rawAst <- getContents
          width  <- getEnv "WIDTH"
          height <- getEnv "HEIGHT"
          mod    <- getEnv "MOD"
          pkg    <- getEnv "PKG"
          names  <- getEnv "NAMES"
          putStrLn (process (read width) (read height) mod pkg (read names) rawAst)
