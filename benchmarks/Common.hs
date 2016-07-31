{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Common where

import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import qualified Data.Scientific as Scientific
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           ML4HSFE.Loop
import           ML4HSFE.Outer

maxN = 10

asts = encode (map mkAst [1..maxN])

mkAst :: Int -> Value
mkAst n = Object (HM.fromList [
      ("ast",          "\"Type\"")
    , ("module",       s)
    , ("package",      s)
    , ("name",         s)
    , ("features",     Array (V.fromList (fs ++ map asNum [n..maxN])))
    , ("dependencies", Array (V.fromList fs))
    ])
  where s  = asStr n
        fs = map (\m -> Object (HM.fromList [
                                   ("name",    asStr m)
                                 , ("module",  asStr m)
                                 , ("package", asStr m)
                                 ]))
                 [1..n]

asStr :: Int -> Value
asStr = String . T.pack . show

asNum :: Int -> Value
asNum = Number . fromInteger . toInteger

together  = clusterLoop  . handleString 10 10
togetherT = clusterLoopT . handle       10 10
