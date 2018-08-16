{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, BangPatterns #-}
module ML4HSFE.Loop where

-- Top-level loop for processing ASTs. We use Haskell since jq+bash is slow.

import qualified Control.DeepSeq            as DS
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import qualified Data.Stringable            as S
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V
import           ML4HSFE
import           ML4HSFE.Types

getAll x = A.decode {-Strict-} x :: Maybe A.Array

ml4hsfe :: Int -> Int -> BS.ByteString -> BS.ByteString
        -> [BS.ByteString] -> LBS.ByteString -> A.Array
ml4hsfe w h mod pkg names rawAst = DS.force vec
  where rawAst'   = LBS.toStrict rawAst
        processed = processVal w h mod pkg names rawAst'
        vec       = V.map featureToVal (V.fromList processed)

featureToVal :: Feature -> A.Value
featureToVal (Left  i)  = DS.force (A.Number (fromInteger . toInteger $ i))
featureToVal (Right id) = fromMaybe (error "Failed to convert ID to JSON value")
                                    (DS.force (A.decode (A.encode id)))

handle :: Int -> Int -> LBS.ByteString -> V.Vector A.Value
handle w h x = case getAll x of
  Nothing  -> error "Failed to parse array of ASTs"
  Just all -> V.map (A.Object . handleOne w h all . unObject) all

handleString :: Int -> Int -> LBS.ByteString -> LBS.ByteString
handleString w h x = A.encode (handle w h x)

unObject (A.Object o) = o
unObject _ = error "Was expecting an object"

unString (A.String s) = s
unString _ = error "Was expecting a string"

unString' :: A.Value -> String
unString' = S.toString . unString

unBS = TE.encodeUtf8 . unString

handleOne :: Int -> Int -> A.Array -> A.Object -> A.Object
handleOne !w !h !xs !x = DS.force (HM.insert "features" (A.Array features) x)
  where features = ml4hsfe w h (unBS mod) (unBS pkg) names'
                           (LBS.fromStrict (unBS ast))
        Just ast = HM.lookup "ast"     x
        Just mod = HM.lookup "module"  x
        Just pkg = HM.lookup "package" x
        names    = V.map (HM.lookup "name" . unObject) (V.filter matchPkgMod xs)
        names'   = V.toList (V.map (\(Just s) -> unBS s) names)
        matchPkgMod y' = let y = unObject y'
                          in (HM.lookup "package" y == Just pkg) &&
                             (HM.lookup "module"  y == Just mod)
