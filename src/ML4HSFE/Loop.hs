{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, BangPatterns #-}
module ML4HSFE.Loop where

-- Top-level loop for processing ASTs. We use Haskell since jq+bash is slow.

import qualified Control.DeepSeq            as DS
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as L
import           Data.Maybe
import qualified Data.Stringable            as S
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V
import qualified Grapher                    as G
import qualified HS2AST.Types               as H
import           ML4HSFE
import           ML4HSFE.Types
import qualified Types                      as Ty

ml4hsfe :: Int -> Int -> BS.ByteString -> BS.ByteString
        -> [BS.ByteString] -> LBS.ByteString -> Features
ml4hsfe w h mod pkg names rawAst = DS.force vec
  where rawAst'   = LBS.toStrict rawAst
        processed :: [Feature]
        processed = processVal w h mod pkg names rawAst'
        vec       = V.fromList processed

handle :: Int -> Int -> LBS.ByteString -> (V.Vector Entry, [[H.Identifier]])
handle !w !h x = case A.decode x of
    Nothing   -> error "Failed to parse array of ASTs"
    Just !all ->  let ids     = L.map Ty.valToAstId (V.toList all)
                      sccs    = L.map G.toIds (G.group ids)
                      entries = V.map (handleOne w h all) all
                   in sccs `DS.deepseq` entries `DS.deepseq` (entries, sccs)

enc = TE.encodeUtf8

handleOne :: Int -> Int -> A.Array -> A.Value -> Entry
handleOne !w !h !xs (A.Object !x) = Entry {
                                      entryCluster       = Nothing,
                                      entryToCluster     = False,
                                      entryFeatures      = Just features,
                                      entryQuickspecable = qs,
                                      entryId            = H.ID {
                                          H.idPackage = pkg,
                                          H.idModule  = mod,
                                          H.idName    = name
                                      }
                                    }
  where !features = ml4hsfe w h (enc mod) (enc pkg) names
                            (LBS.fromStrict (enc ast))
        Just (A.String !ast ) = HM.lookup "ast"     x
        Just (A.String !name) = HM.lookup "name"    x
        Just (A.String !mod ) = HM.lookup "module"  x
        Just (A.String !pkg ) = HM.lookup "package" x
        qs                    = case HM.lookup "quickspecable" x of
                                  Just (A.Bool !q) -> q
                                  Nothing          -> False
        names    = L.map getName (L.filter matchPkgMod (V.toList xs))
        getName (A.Object o) = case HM.lookup "name" o of
                                 Just (A.String !s) -> TE.encodeUtf8 s

        -- TODO: Why are we limiting things to one package/module?
        matchPkgMod (A.Object y) =
          (HM.lookup "package" y == Just (A.String pkg)) &&
          (HM.lookup "module"  y == Just (A.String mod))
