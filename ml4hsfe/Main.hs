module Main where

import qualified Data.ByteString.Char8 as BS
import           ML4HSFE
import           System.Environment
import           System.IO

main = do rawAst <- BS.getContents
          width  <- getEnv "WIDTH"
          height <- getEnv "HEIGHT"
          mod    <- getEnv "MOD"
          pkg    <- getEnv "PKG"
          names  <- getEnv "NAMES"
          BS.putStrLn (process (read width) (read height) (BS.pack mod) (BS.pack pkg) (read names) rawAst)
