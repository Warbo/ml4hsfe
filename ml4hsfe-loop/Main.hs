module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           ML4HSFE.Loop
import           System.Environment
import           System.IO

main = do rawAsts <- BL.getContents
          width   <- getEnv "WIDTH"
          height  <- getEnv "HEIGHT"
          BS.putStrLn (BL.toStrict (encode (handle (read width) (read height) rawAsts)))
