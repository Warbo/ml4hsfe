{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Suites.ML4HSFE where

import qualified CoreSyn             as C
import           Data.Aeson
import qualified Data.AttoLisp       as L
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Set            as S
import           Data.String
import qualified Data.Stringable     as Str
import qualified Data.Vector         as V
import           FastString
import           Generators
import           Helpers
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


tests = testGroup "Feature extraction tests" [
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
  , testProperty "Qualify module locals to globals" qualifyMods
  , testProperty "Vectors contain globals from mod" globalVecContents
  , testProperty "Module contents are global"       globalModContents
  ]

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

canExtractFeatures (Positive r) (Positive c) e =
  length (featureVec c r e) == c * r

evalExprs = walkE

exprsGetTrees ctx e = walkTree (toTree ctx e)

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

walkL (L !x) = True

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

qualifyMods pre post = forAll mkId check
  where check i = g `isIn` qualifyMod m p [Str.fromString (idName i)] (L.List (pre ++ [l] ++ post))
          where m = L.String (Str.fromString (idModule  i))
                p = L.String (Str.fromString (idPackage i))
                g = asGlobal i
                l = asLocal  i
                isIn :: L.Lisp -> L.Lisp -> Bool
                isIn x y | x == y = True
                isIn x (L.List xs) | x `elem` xs = True
                isIn x (L.List xs) = any (x `isIn`) xs

globalsIncluded x (Positive r) (Positive c) =
  forAll safeId (globalsIncluded' x r c)

globalsIncluded' x r c g = Str.toString (encode g) `isInfixOf` vec
  where vec = renderVector (featureVec (r + 30) (c + 30) (App x (Var (Global (G g)))))

globalVecContents padN (Positive n) =
    forAll mkId checkContents
  where checkContents i = forAll (mkLisp (n `mod` 20) (asLocal i))
                                 globalModContents'
          where globalModContents' l =
                    counterexample (show ("vec",   vec,
                                          "names", names,
                                          "expr",  expr)) $
                      Right i `elem` vec
                  where names = fst padN ++ [idName i] ++ snd padN
                        expr  = qualifyExpr (sToL (idModule  i))
                                            (sToL (idPackage i))
                                            (map Str.fromString names)
                                            l
                        vec   = featureVec 30 30 expr

globalModContents padN (Positive n) =
    forAll mkId checkContents
  where checkContents i = forAll (mkLisp (n `mod` 20) (asLocal i))
                                 globalModContents'

          where globalModContents' l = counterexample vec $
                                         Str.toString (encode i) `isInfixOf` vec
                  where names = fst padN ++ [idName i] ++ snd padN
                        vec   = process 30
                                        30
                                        (idModule  i)
                                        (idPackage i)
                                        names
                                        (Str.toString (L.encode l))
