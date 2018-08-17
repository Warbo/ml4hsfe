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
import qualified HS2AST.Types               as H
import           ML4HSFE
import           ML4HSFE.Types

ml4hsfe :: Int -> Int -> BS.ByteString -> BS.ByteString
        -> [BS.ByteString] -> LBS.ByteString -> A.Array
ml4hsfe w h mod pkg names rawAst = DS.force vec
  where rawAst'   = LBS.toStrict rawAst
        processed = processVal w h mod pkg names rawAst'
        vec       = V.map featureToVal (V.fromList processed)

featureToVal :: Feature -> A.Value
featureToVal (Left  !i)  = A.Number (fromInteger (toInteger i))
featureToVal (Right !id) = let !name = H.idName    id
                               !mod  = H.idModule  id
                               !pkg  = H.idPackage id
                            in A.Object (HM.fromList [
                                 (   "name", A.String name),
                                 ( "module", A.String mod ),
                                 ("package", A.String pkg )])

handle :: Int -> Int -> LBS.ByteString -> V.Vector A.Value
handle w h x = case A.decode x of
  Nothing  -> error "Failed to parse array of ASTs"
  Just all -> V.map (handleOne w h all) all

unString (A.String s) = s
unString _ = error "Was expecting a string"

unBS = TE.encodeUtf8 . unString

handleOne :: Int -> Int -> A.Array -> A.Value -> A.Value
handleOne !w !h !xs (A.Object !x) = A.Object result
  where !result   = HM.insert "features" (A.Array features) x
        !features = ml4hsfe w h (unBS mod) (unBS pkg) names
                            (LBS.fromStrict (unBS ast))
        Just !ast = HM.lookup "ast"     x
        Just !mod = HM.lookup "module"  x
        Just !pkg = HM.lookup "package" x
        names    = L.map getName (L.filter matchPkgMod (V.toList xs))
        getName (A.Object o) = case HM.lookup "name" o of
                                 Just (A.String !s) -> TE.encodeUtf8 s
        matchPkgMod (A.Object y) = (HM.lookup "package" y == Just pkg) &&
                                   (HM.lookup "module"  y == Just mod)
