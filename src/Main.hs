{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson

import qualified Data.ByteString.Lazy as B
import           Grapher
import           Jparse

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> B.getContents) :: IO (Either String [ASTId])
 case d of
  Left err -> putStrLn err
  Right ps -> putStrLn . show $ map extractGraphable ps
