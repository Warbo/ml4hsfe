{-# LANGUAGE PartialTypeSignatures, OverloadedStrings #-}
module ML4HSFE.Outer where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Stringable as S
import qualified Data.Vector as V
import System.Process

type ASTs = Array
type SCC  = Array

fromRight (Right x) = x
fromRight (Left  e) = error e

clusterLoop :: String -> IO ASTs
clusterLoop s = do sccsStr <- order (S.toString s)
                   clusterSCCs asts (fromRight (eitherDecode' (S.fromString sccsStr)))
  where asts = fromRight (eitherDecode' (S.fromString s))

clusterSCCs :: ASTs -> [SCC] -> IO ASTs
clusterSCCs asts []         = return asts
clusterSCCs asts (scc:sccs) = clusterSCCs (enableScc asts scc) sccs

enableScc :: ASTs -> SCC -> ASTs
enableScc asts s' =
    if V.null s'
       then asts
       else fromRight . (`parseEither` V.head s') $ withObject "Non-object in SCC"
                                                               (enable (V.tail s'))
  where enable ss s = do
          name <- s .: "name"
          mod  <- s .: "module"
          pkg  <- s .: "package"
          return (enableScc (V.map (enableMatching name mod pkg) asts) ss)

enableMatching :: String -> String -> String -> _ -> _
enableMatching name mod pkg x' = fromRight . (`parseEither` x') $ withObject "Need object for AST" go
  where go x = do
          n <- x .: "name"
          m <- x .: "module"
          p <- x .: "package"
          return . Object $ if n == name && m == mod && p == pkg
                      then HM.insert "tocluster" (Bool True) x
                      else x

order :: String -> IO String
order = readProcess "order-deps" []

renderAsts :: ASTs -> String
renderAsts = S.toString . encode
