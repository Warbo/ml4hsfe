module Main where

import qualified Data.ByteString.Lazy as B
import           Grapher

main :: IO ()
main = do c <- B.getContents
          let ps = parse c
          print (process ps)
