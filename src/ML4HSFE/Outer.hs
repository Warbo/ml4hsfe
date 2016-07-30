{-# LANGUAGE PartialTypeSignatures, OverloadedStrings, DeriveGeneric #-}
module ML4HSFE.Outer where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import qualified Data.Scientific            as Sci
import qualified Data.Stringable            as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics (Generic)
import qualified Grapher                    as OD -- From order-deps
import qualified HS2AST.Types               as H
import           System.Environment
import           System.IO.Unsafe
import           System.Process

type ASTs   = Array
type SCC    = Array
type ID     = (Name, Module, Package)
type Prop a = HM.HashMap ID a

newtype Name      = N T.Text deriving (Show, Eq, Generic)
newtype Module    = M T.Text deriving (Show, Eq, Generic)
newtype Package   = P T.Text deriving (Show, Eq, Generic)
newtype ClusterID = C Sci.Scientific deriving (Show, Eq, Generic)

instance Hashable Name
instance Hashable Module
instance Hashable Package

fromRight (Right x) = x
fromRight (Left  e) = error e

clusterLoop :: String -> IO ASTs
clusterLoop s = clusterSCCs asts (fromRight (eitherDecode' (S.fromString sccsStr)))
  where asts    = fromRight (eitherDecode' (S.fromString s))
        sccsStr = order (S.toString s)

clusterLoopT :: _ -> IO ASTs
clusterLoopT s = clusterSCCsT asts sccs
  where asts = s
        sccs = map OD.toIds .
               OD.group     .
               V.toList     .
               V.map (fromRight . parseEither parseJSON) $ s

clusterSCCs :: ASTs -> [SCC] -> IO ASTs
clusterSCCs asts []         = return asts
clusterSCCs asts (scc:sccs) = do c <- regularCluster (en scc)
                                 clusterSCCs c sccs
  where en = enableScc asts

clusterSCCsT :: ASTs -> [_] -> IO ASTs
clusterSCCsT asts []         = return asts
clusterSCCsT asts (scc:sccs) = do c <- regularCluster (en scc)
                                  clusterSCCsT c sccs
  where en = enableSccT asts

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
          return (enableScc (V.map (enableMatching (N name) (M mod) (P pkg)) asts) ss)

enableSccT :: ASTs -> _ -> ASTs
enableSccT asts s' =
    if null s'
       then asts
       else enable (tail s') (head s')
  where enable ss s =
          let [name, mod, pkg] = map T.pack [H.idName    s,
                                             H.idModule  s,
                                             H.idPackage s]
           in enableSccT (V.map (enableMatching (N name)
                                                (M mod)
                                                (P pkg))
                                asts)
                         ss

enableMatching :: Name -> Module -> Package -> _ -> _
enableMatching (N name) (M mod) (P pkg) x' = fromRight . (`parseEither` x') $ withObject "Need object for AST" go
  where go x = do
          n <- x .: "name"
          m <- x .: "module"
          p <- x .: "package"
          return . Object $ if n == name && m == mod && p == pkg
                      then HM.insert "tocluster" (Bool True) x
                      else x

order :: String -> String
order = S.toString . OD.process . OD.parse . S.fromString

renderAsts :: ASTs -> String
renderAsts = S.toString . encode

-- Set the features to whatever cluster numbers appear in asts, run Weka on
-- these finalised features, then splice the resulting numbers back into the
-- original asts array (so we can set new values for the features next time)
regularCluster :: ASTs -> IO ASTs
regularCluster asts = do clusterNums <- runWeka withFeatures
                         return (setClustersFrom asts clusterNums)
  where withFeatures = setOwnFeatures asts

setOwnFeatures :: ASTs -> ASTs
setOwnFeatures asts = let clusters = readAsts asts "cluster" (C . unNum)
                       in V.map (setFeaturesFrom clusters) asts

unNum (Number n) = n

readAsts :: ASTs -> _ -> (Value -> a) -> Prop a
readAsts asts key f = V.foldl' add HM.empty asts
  where add ps (Object o) = case HM.lookup key o of
          Nothing -> ps
          Just v  -> HM.insert (idOf o) (f v) ps

idOf :: Object -> ID
idOf x = fromJust $ do
  (n, m, p) <- getNMP x
  return (N n, M m, P p)

{-# NOINLINE runWekaCmd #-}
runWekaCmd :: String
runWekaCmd = unsafePerformIO $ do
  cmd <- lookupEnv "RUN_WEKA_CMD"
  case cmd of
    Just c  -> return c
    Nothing -> return "runWeka"

runWeka :: ASTs -> IO (Prop ClusterID)
runWeka asts = do
    stdout <- readProcess runWekaCmd [] stdin
    return (toClusters (parse stdout))
  where stdin = S.toString (encode asts)
        parse         = fromRight . eitherDecode' . S.fromString
        toClusters xs = readAsts xs "cluster" (C . unNum)

setClustersFrom :: ASTs -> Prop ClusterID -> ASTs
setClustersFrom into from = V.map (`setClusterFrom` from) into

setClusterFrom :: Value -> Prop ClusterID -> Value
setClusterFrom (Object ast) clusters = Object $ case go of
    Nothing    -> ast
    Just (C c) -> HM.insert "cluster" (Number c) ast
  where go = do
          (n, m, p) <- getNMP ast
          HM.lookup (N n, M m, P p) clusters

getNMP x = do String n <- HM.lookup "name"    x
              String m <- HM.lookup "module"  x
              String p <- HM.lookup "package" x
              return (n, m, p)

setFeaturesFrom :: Prop ClusterID -> Value -> Value
setFeaturesFrom clusters (Object ast) =
  Object $ case HM.lookup "features" ast of
    Nothing         -> ast
    Just (Array fv) -> HM.insert "features"
                                 (Array (V.map (setFVFrom clusters) fv))
                                 ast

setFVFrom :: Prop ClusterID -> Value -> Value
setFVFrom clusters (Number n) = Number n
setFVFrom clusters (Object f) = Number . (300 +) $ case new of
                                                    Nothing    -> 0
                                                    Just (C n) -> n
  where new = do
          (n, m, p) <- getNMP f
          HM.lookup (N n, M m, P p) clusters

findAst :: ASTs -> Name -> Module -> Package -> _ -> Maybe Value
findAst asts (N name) (M mod) (P pkg) key = V.find ((/= Nothing) . check) asts
  where check (Object x) = do
          (n, m, p) <- getNMP x
          val       <- HM.lookup key       x
          if n == name && m == mod && p == pkg
             then Just val
             else Nothing
