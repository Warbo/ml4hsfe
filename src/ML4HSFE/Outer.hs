{-# LANGUAGE BangPatterns, ConstraintKinds, DeriveGeneric, FlexibleContexts,
             OverloadedStrings, PartialTypeSignatures, RankNTypes #-}
module ML4HSFE.Outer where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import qualified Grapher                    as G
import qualified HS2AST.Types               as H
import qualified ML4HSFE.Loop               as Loop
import qualified ML4HSFE.Types              as Ty
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           System.Process
import qualified System.Process.ByteString.Lazy as SBS

type ASTs   = V.Vector Ty.Entry
type AST    = Value
type SCC    = [H.Identifier]
type Prop a = HM.HashMap H.Identifier a

newtype Name    = N T.Text deriving (Show, Eq, Generic)
newtype Module  = M T.Text deriving (Show, Eq, Generic)
newtype Package = P T.Text deriving (Show, Eq, Generic)
type ClusterID  = Int

instance Hashable Name
instance Hashable Module
instance Hashable Package

instance Hashable H.Identifier where
  hashWithSalt salt i = salt `hashWithSalt` H.idName    i
                             `hashWithSalt` H.idModule  i
                             `hashWithSalt` H.idPackage i

type Clusterer = ASTs -> Prop ClusterID

clusterLoop :: Clusterer -> ASTs -> [SCC] -> ASTs
clusterLoop f = L.foldl' go
  where go :: ASTs -> SCC -> ASTs
        go asts scc = regularCluster (enableScc asts scc)

        -- Set the features to whatever cluster numbers appear in asts, run
        -- clusterer on these finalised features, then splice the resulting
        -- numbers back into the original asts array (so we can set new values
        -- for the features next time)
        regularCluster :: ASTs -> ASTs
        regularCluster !asts = V.map (`setClusterFrom` f (setOwnFeatures asts)) asts

setOwnFeatures :: ASTs -> ASTs
setOwnFeatures !asts = let !clusters = toClusters $! asts
                        in V.map (setFeaturesFrom clusters) asts

toClusters :: ASTs -> Prop ClusterID
toClusters = V.foldl' add HM.empty
  where add ps e = case Ty.entryCluster e of
                     Nothing -> ps
                     Just !v -> let !i = Ty.entryId e
                                 in HM.insert i v ps

enableScc :: ASTs -> SCC -> ASTs
enableScc = L.foldl' go
  where go :: ASTs -> H.Identifier -> ASTs
        go !asts i = V.map (enableMatching i) asts

enableMatching :: H.Identifier -> Ty.Entry -> Ty.Entry
enableMatching i x = if i == Ty.entryId x
                        then x { Ty.entryToCluster = True }
                        else x

setClusterFrom :: Ty.Entry -> Prop ClusterID -> Ty.Entry
setClusterFrom !ast clusters = ast {
    Ty.entryCluster = HM.lookup (Ty.entryId ast) clusters
  }


getNMP x = do String !n <- HM.lookup "name"    x
              String !m <- HM.lookup "module"  x
              String !p <- HM.lookup "package" x
              return $! (n, m, p)

unNum (Number !n) = n
unNum x           = error (show ("Expected 'Number'", x))

fromLeft (Left  x) = x
fromLeft (Right _) = error "Expected a Left"

pureKmeans :: Maybe Int -> Clusterer
pureKmeans cs asts = toClusters (if num == 0 then V.empty else go)
  where go :: ASTs
        go = snd (L.foldl' concatClusters
                           (0, V.empty)
                           (K.kmeansGen toFeatures clusters getCsv))

        toFeatures :: Ty.Entry -> [Double]
        toFeatures o = case Ty.entryFeatures o of
                         Nothing -> error "No features"
                         Just a  -> map (fromIntegral . fromLeft) (V.toList a)

        concatClusters :: (Int, ASTs) -> [Ty.Entry] -> (Int, ASTs)
        concatClusters (!n, !asts) c =
          (n+1, asts V.++ V.fromList (map (addCluster n) c))

        addCluster :: Int -> Ty.Entry -> Ty.Entry
        addCluster n e = e { Ty.entryCluster = Just (n+1) }

        inlines :: ASTs
        inlines = V.filter keep asts

        keep :: Ty.Entry -> Bool
        keep o = case Ty.entryFeatures o of
          Nothing -> False
          Just _  -> Ty.entryToCluster o

        num :: Int
        num = length inlines

        clusters :: Int
        clusters = fromMaybe (ceiling (sqrt (fromIntegral num / 2))) cs

        getCsv :: [Ty.Entry]
        getCsv = filter clusterable (V.toList inlines)

        clusterable x = case Ty.entryFeatures x of
                          Nothing -> False
                          Just _  -> True

setFeaturesFrom :: Prop ClusterID -> Ty.Entry -> Ty.Entry
setFeaturesFrom clusters ast = case Ty.entryFeatures ast of
    Nothing -> ast
    Just fv -> ast { Ty.entryFeatures = Just (V.map (setFVFrom clusters) fv) }

setFVFrom :: Prop ClusterID -> Ty.Feature -> Ty.Feature
setFVFrom _        (Left  n) = Left n
setFVFrom clusters (Right i) = Left (300 + (case HM.lookup i clusters of
                                             Nothing -> 0
                                             Just n  -> n))

outerMain :: IO ()
outerMain = do
  rawAsts  <- LBS.getContents
  width    <- read <$> getEnv "WIDTH"
  height   <- read <$> getEnv "HEIGHT"
  clusters <- lookupEnv "CLUSTERS"
  let (!asts, !sccs) = Loop.handle width height rawAsts
      asts' = clusterLoop (pureKmeans (read <$> clusters)) asts sccs
  LBS.putStrLn (encode asts')
