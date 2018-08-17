{-# LANGUAGE BangPatterns, ConstraintKinds, DeriveGeneric, FlexibleContexts,
             OverloadedStrings, PartialTypeSignatures, RankNTypes #-}
module ML4HSFE.Outer where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Functor.Identity      as I
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.KMeans                as K
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Scientific            as Sci
import qualified Data.Stringable            as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics (Generic)
import qualified Grapher                    as OD -- From order-deps
import qualified HS2AST.Types               as H
import qualified ML4HSFE.Loop               as Loop
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           System.Process
import qualified System.Process.ByteString.Lazy as SBS

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

type Clusterer = ASTs -> Prop ClusterID

fromRight (Right x) = x
fromRight (Left  e) = error e

clusterLoop :: Clusterer -> ASTs -> ASTs
clusterLoop f !asts = clusterSCCs f asts (fromRight (eitherDecode' sccsStr))
  where !sccsStr = case fromJSON (Array asts) of
                     Error   msg -> error msg
                     Success ids -> OD.process ids

clusterSCCs :: Clusterer -> ASTs -> [SCC] -> ASTs
clusterSCCs f = L.foldl' go
  where go !asts scc = regularCluster f (enableScc asts scc)

enableScc :: ASTs -> SCC -> ASTs
enableScc = V.foldl' go
  where go :: ASTs -> Value -> ASTs
        go !asts (Object !s) = let Just (String !name) = HM.lookup "name"    s
                                   Just (String !mod ) = HM.lookup "module"  s
                                   Just (String !pkg ) = HM.lookup "package" s
                                   f = enableMatching (N name) (M mod) (P pkg)
                                in V.map f asts

enableMatching :: Name -> Module -> Package -> Value -> Value
enableMatching (N !name) (M !mod) (P !pkg) (Object !x) = Object o
  where Just (String !n) = HM.lookup "name"    x
        Just (String !m) = HM.lookup "module"  x
        Just (String !p) = HM.lookup "package" x
        !o = if n == name && m == mod && p == pkg
                then HM.insert "tocluster" (Bool True) x
                else x

order :: LBS.ByteString -> LBS.ByteString
order = OD.process . OD.parse

renderAsts :: ASTs -> String
renderAsts = S.toString . encode

-- Set the features to whatever cluster numbers appear in asts, run clusterer on
-- these finalised features, then splice the resulting numbers back into the
-- original asts array (so we can set new values for the features next time)
regularCluster :: Clusterer -> ASTs -> ASTs
regularCluster f !asts = setClustersFrom asts clusters
  where !withFeatures = setOwnFeatures asts
        !clusters     = f withFeatures

setOwnFeatures :: ASTs -> ASTs
setOwnFeatures !asts = let clusters = readAsts asts "cluster" (C . unNum)
                        in V.map (setFeaturesFrom clusters) asts

unNum (Number !n) = n
unNum x           = error (show ("Expected 'Number'", x))

readAsts :: ASTs -> T.Text -> (Value -> a) -> Prop a
readAsts asts key f = V.foldl' add HM.empty asts
  where add ps (Object o) = case HM.lookup key o of
          Nothing -> ps
          Just v  -> HM.insert (idOf o) (f v) ps

idOf :: Object -> ID
idOf x = fromJust $ do
  (!n, !m, !p) <- getNMP x
  return $! (N n, M m, P p)

pureKmeans :: Maybe Int -> Clusterer
pureKmeans cs asts = toClusters (if num == 0 then V.empty else go)
  where go :: ASTs
        go = snd (L.foldl' concatClusters
                           (0, V.empty)
                           (K.kmeansGen toFeatures clusters getCsv))

        toFeatures :: Value -> [Double]
        toFeatures (Object o) =
          case HM.lookupDefault (error "No 'features'") "features" o of
            Array a -> map (Sci.toRealFloat . unNum) (V.toList a)
            x       -> error (show ("Got the following 'features'", x))

        concatClusters :: (Int, ASTs) -> [Value] -> (Int, ASTs)
        concatClusters (!n, !asts) c =
          (n+1, asts V.++ V.fromList (map (addCluster n) c))

        addCluster :: Int -> Value -> Value
        addCluster n (Object o) = Object (HM.insert "cluster"
                                                    (Number (fromIntegral (n+1)))
                                                    o)

        inlines :: ASTs
        inlines = V.filter keep asts

        keep :: Value -> Bool
        keep (Object o) = HM.member "features" o &&
                          case HM.lookupDefault (Bool False) "tocluster" o of
                            Bool b -> b
        keep _          = False

        num :: Int
        num = length inlines

        clusters :: Int
        clusters = fromMaybe (ceiling (sqrt (fromIntegral num / 2))) cs

        getCsv :: [Value]
        getCsv = filter clusterable (V.toList inlines)

        clusterable x =
          case x of
            Object o -> case HM.lookupDefault (Array V.empty) "features" o of
                          Array a -> not (V.null a)
                          y       -> error (show ("'features' should be array",
                                                 y))
            _        -> error (show ("ASTs should be objects", x))

toClusters xs = readAsts xs "cluster" (C . unNum)

setClustersFrom :: ASTs -> Prop ClusterID -> ASTs
setClustersFrom !into from = let !v = V.map (`setClusterFrom` from) into
                              in v

setClusterFrom :: Value -> Prop ClusterID -> Value
setClusterFrom (Object !ast) clusters = Object ast'
  where !ast' = case go of
                  Nothing     -> ast
                  Just (C !c) -> HM.insert "cluster" (Number c) ast
        go = do (n, m, p) <- getNMP ast
                HM.lookup (N n, M m, P p) clusters

getNMP x = do String !n <- HM.lookup "name"    x
              String !m <- HM.lookup "module"  x
              String !p <- HM.lookup "package" x
              return $! (n, m, p)

setFeaturesFrom :: Prop ClusterID -> Value -> Value
setFeaturesFrom clusters (Object ast) =
  Object $ case HM.lookup "features" ast of
    Nothing         -> ast
    Just (Array fv) -> HM.insert "features"
                                 (Array (V.map (setFVFrom clusters) fv))
                                 ast

setFVFrom :: Prop ClusterID -> Value -> Value
setFVFrom _        (Number n) = Number n
setFVFrom clusters (Object f) = Number . (300 +) $ case new of
                                                    Nothing    -> 0
                                                    Just (C n) -> n
  where new = do
          (n, m, p) <- getNMP f
          HM.lookup (N n, M m, P p) clusters

findAst :: ASTs -> Name -> Module -> Package -> T.Text -> Maybe Value
findAst asts (N name) (M mod) (P pkg) key = V.find ((/= Nothing) . check) asts
  where check (Object x) = do
          (n, m, p) <- getNMP x
          val       <- HM.lookup key       x
          if n == name && m == mod && p == pkg
             then Just val
             else Nothing

outerMain :: IO ()
outerMain = do
  rawAsts  <- LBS.getContents
  width    <- getEnv "WIDTH"
  height   <- getEnv "HEIGHT"
  clusters <- lookupEnv "CLUSTERS"
  let asts  = Loop.handle (read width) (read height) rawAsts
      asts' = clusterLoop (pureKmeans (read <$> clusters)) asts
  putStrLn (renderAsts asts')
