{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson

import qualified Data.ByteString.Lazy as B
import           Jparse

main :: IO ()
main = do c <- B.getContents
          let ps = parse c
          print (process ps)
