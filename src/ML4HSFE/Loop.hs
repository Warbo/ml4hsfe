{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}
module ML4HSFE.Loop where

-- Top-level loop for processing ASTs. We use Haskell since jq+bash is slow.

import qualified Data.Aeson          as A
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Stringable     as S
import           ML4HSFE
import           ML4HSFE.Types

getAll x = A.decode x :: Maybe A.Array

ml4hsfe :: Int -> Int -> _ -> _ -> _ -> _ -> A.Array
ml4hsfe w h mod pkg names rawAst = V.fromList (map featureToVal (processVal w h mod pkg names rawAst))

featureToVal :: Feature -> A.Value
featureToVal (Left  i)  = A.Number (fromInteger . toInteger $ i)
featureToVal (Right id) = case A.decode (A.encode id) of
  Nothing -> error "Failed to convert ID to JSON value"
  Just x  -> x

handle w h x = case getAll x of
  Nothing  -> error "Failed to parse array of ASTs"
  Just all -> V.map (handleOne w h all . unObject) all

handleString :: Int -> Int -> String -> String
handleString w h x = S.toString (A.encode (handle w h (S.fromString x)))

unObject (A.Object o) = o
unObject _ = error "Was expecting an object"

unString (A.String s) = s
unString _ = error "Was expecting a string"

unString' :: A.Value -> String
unString' = S.toString . unString

handleOne :: Int -> Int -> A.Array -> A.Object -> A.Object
handleOne w h xs x = HM.insert "features" (A.Array features) x
  where features = ml4hsfe w h (unString' mod) (unString' pkg) names' (S.toString (A.encode ast))
        Just ast = HM.lookup "ast"     x
        Just mod = HM.lookup "module"  x
        Just pkg = HM.lookup "package" x
        names    = V.map (HM.lookup "name" . unObject) (V.filter matchPkgMod xs)
        names'   = V.toList (V.map (\(Just s) -> unString' s) names)
        matchPkgMod y' = let y = unObject y'
                          in (HM.lookup "package" y == Just pkg) &&
                             (HM.lookup "module"  y == Just mod)
