{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Suites.ML4HSFE where

import qualified CoreSyn             as C
import           Data.Aeson
import qualified Data.AttoLisp       as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.String
import qualified Data.Text.Encoding  as TE
import           FastString
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
import           Test.Tasty             (testGroup)
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
        length' (L.List ["(:)", _, xs]) = 1 + length' xs
        length' _                       = error "Invalid uninstated list"

syntaxMatches :: String -> C.CoreExpr -> Bool
syntaxMatches _ = evalTree . readExpr . toSexp (const (Just ""))

evalTree Nothing  = error "Got no tree"
evalTree (Just x) = walkE x

canReadLocalVars x =
  case readLocal (L.List ["name", L.String x]) of
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

walkL (L !_) = True

walkB (NonRec x)  = walkBinder x
walkB (Rec    xs) = all walkBinder xs

walkBinder (Bind x y) = walkL x && walkE y

walkLit LitNum = True
walkLit LitStr = True

walkA (Alt ac e ls) = walkAC ac && walkE e && all walkL ls

walkG (G _) = True

walkAC a = case a of
  DataAlt x -> walkC   x
  LitAlt  x -> walkLit x
  Default   -> True

instance IsString PackageName where
  fromString = PackageName . mkFastString

qualifyMods pre post = forAll mkId check
  where check i = g `isIn` qualifyMod m p [idName i] (L.List (pre ++ [l] ++ post))
          where m = L.String (idModule  i)
                p = L.String (idPackage i)
                g = asGlobal i
                l = asLocal  i
                isIn :: L.Lisp -> L.Lisp -> Bool
                isIn x y | x == y = True
                isIn x (L.List xs) | x `elem` xs = True
                isIn x (L.List xs) = any (x `isIn`) xs

globalsIncluded x (Positive r) (Positive c) =
  forAll safeId (globalsIncluded' x r c)

globalsIncluded' x r c g = BL.toStrict (encode g) `BS.isInfixOf` vec
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
                        expr  = qualifyExpr (L.String (idModule  i))
                                            (L.String (idPackage i))
                                            names
                                            l
                        vec   = featureVec 30 30 expr

globalModContents padN (Positive n) =
    forAll mkId checkContents
  where checkContents i = forAll (mkLisp (n `mod` 20) (asLocal i))
                                 globalModContents'

          where globalModContents' l = counterexample (BS.unpack vec) $
                                         BL.toStrict (encode i) `BS.isInfixOf` vec
                  where names = fst padN ++ [idName i] ++ snd padN
                        vec   = process 30
                                        30
                                        (TE.encodeUtf8 (idModule  i))
                                        (TE.encodeUtf8 (idPackage i))
                                        (map TE.encodeUtf8 names)
                                        (BL.toStrict (L.encode l))
