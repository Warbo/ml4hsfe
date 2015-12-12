{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified CoreSyn             as C
import           Data.Aeson
import qualified Data.AttoLisp       as L
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Set            as S
import           Data.String
import qualified Data.Stringable     as Str
import qualified Data.Vector         as V
import           FastString
import           Generators
import           HS2AST.Sexpr
import           HS2AST.Types hiding (Node)
import           ML4HSFE             as FE
import           ML4HSFE.FeatureExtraction
import           ML4HSFE.Generators
import           ML4HSFE.Parse
import           ML4HSFE.Types
import           PackageConfig
import           Test.QuickCheck
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Syntax is expected"               syntaxMatches
  , testProperty "Unwrap ASTs"                      canUnwrapAsts
  -- TODO: This works, but discards too many inputs to pass
  --, testProperty "Reinstate lists"                  canReinstateLists
  , testProperty "Can read local vars"              canReadLocalVars
  , testProperty "Expressions get trees"            exprsGetTrees
  , testProperty "Can extract levels"               canExtractLevels
  , testProperty "Can extract features"             canExtractFeatures
  , testProperty "Can force arbitrary expressions"  evalExprs
  , testProperty "Globals included"                 globalsIncluded
  ]

mapId f (ID x y z) = ID (f x) (f y) (f z)

typesErased = isNothing (readExpr (L.String "Type"))

hasType (L.String s)  = s `elem` ["Type", "TyCon", "TyConApp", "Tick",
                                  "TyVarTy", "Cast", "Coercion"]
hasType (L.List   zs) = any hasType zs

canUnwrapAsts ast = noNest (unwrapAst ast)
  where noNest (L.List [L.List _]) = False
        noNest (L.List xs)         = all noNest xs
        noNest _                   = True

canReinstateLists :: C.CoreExpr -> Bool
canReinstateLists e = case getRecs (toSexp (const Nothing) e) of
    (L.List ["Rec", x]:_) -> case asList x of
      Nothing -> False
      Just x' -> valid x x'
    (L.List ("Rec":_):_)  -> error "Unexpected Rec form"
    [] -> discard
  where getRecs x@(L.List ("Rec":_)) = [x]
        getRecs   (L.List xs)        = concatMap getRecs xs
        getRecs   _                  = []
        valid x x' = length x' == length' x
        length' (L.List ["[]"])         = 0
        length' (L.List ["(:)", x, xs]) = 1 + length' xs
        length' _                       = error "Invalid uninstated list"

syntaxMatches :: String -> C.CoreExpr -> Bool
syntaxMatches p = evalTree . readExpr . toSexp (const (Just ""))

evalTree Nothing  = error "Got no tree"
evalTree (Just x) = walkE x

canReadLocalVars x =
  case readLocal (L.List ["name", L.String (Str.fromString x)]) of
    Nothing     -> error "Failed to read"
    Just (L x') -> x == x'

canExtractLevels (Positive l) f = level l (tree l) == [f]
  where tree 1 = Node f []
        tree n = Node (Left (n `mod` 3)) [tree (n-1)]

canExtractFeatures (Positive r) (Positive c) (EO (e, cs)) =
  length (featureVec c r cs e) == c * r

evalExprs (EO (e, cs)) = walkE e

exprsGetTrees ctx (EO (e, cs)) = walkTree (toTree cs ctx e)

walkTree (Node x xs) = (case x of
  Left  _ -> id
  Right _ -> id) $ all walkTree xs

walkE e = case e of
  Var  x      -> walkI   x
  Lit  x      -> walkLit x
  App  x y    -> walkE   x && walkE y
  Lam  x y    -> walkL   x && walkE y
  Let  x y    -> walkB   x && walkE y
  Case x y zs -> walkE   x && walkL y && all walkA zs
  Type        -> True

walkI i = case i of
  Local       x -> walkL x
  Global      x -> walkG x
  Constructor x -> walkC x

walkC () = True

walkL (L x) = length x >= 0

walkB (NonRec x)  = walkBinder x
walkB (Rec    xs) = all walkBinder xs

walkBinder (Bind x y) = walkL x && walkE y

walkLit LitNum = True
walkLit LitStr = True

walkA (Alt ac e ls) = walkAC ac && walkE e && all walkL ls

walkG (G x) = True

walkAC a = case a of
  DataAlt x -> walkC   x
  LitAlt  x -> walkLit x
  Default   -> True

instance IsString PackageName where
  fromString = PackageName . mkFastString

globalsIncluded (EO (x, cs)) (Positive r) (Positive c) =
  forAll safeId (globalsIncluded' x cs r c)

globalsIncluded' x cs r c g = Str.toString (encode g) `isInfixOf` vec
  where vec = renderVector (featureVec (r + 30) (c + 30) cs (App x (Var (Global (G g)))))
