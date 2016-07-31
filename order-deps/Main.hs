module Main where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Grapher

main :: IO ()
main = do c <- B.getContents
          let ps = parse c
          BC.putStrLn (process ps)
