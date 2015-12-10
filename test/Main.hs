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
    testProperty "Can extract IDs from AST"         canExtractIds
  , testProperty "Matrix rows same length"          matricesLineUp
  , testProperty "Rows merge properly"              rowsMerge
  , testProperty "Spot IDs when building matrix"    matricesHaveIds
  , testProperty "IDs get substituted"              canSubIds
  , testProperty "Can turn AST into feature matrix" canGetFeatures
  , testProperty "Can lookup JSON clusters"         canLookupClusters
  , testProperty "Example JSON works"               exampleCluster
  , testProperty "Default feature is used"          defaultFeatureUsed
  , testProperty "Matrix fits width"                matrixFitsWidth
  , testProperty "Matrix fits height"               matrixFitsHeight
  , testProperty "Matrix rendered to line"          matrixRenderedToLine
  , testProperty "Types erased"                     typesErased
  , testProperty "Syntax is expected"               syntaxMatches
  , testProperty "Unwrap ASTs"                      canUnwrapAsts
  , testProperty "Reinstate lists"                  canReinstateLists
  , testProperty "Can read local vars"              canReadLocalVars
  , testProperty "Expressions get trees"            exprsGetTrees
  ]

canExtractIds ids = forAll (sexprWith ids) canExtract
  where canExtract expr = ids' `S.isSubsetOf` S.fromList (extractIds expr)
        ids'            = S.fromList (map cleanId ids)

mapId f (ID x y z) = ID (f x) (f y) (f z)

cleanId = mapId filterLisp

matricesLineUp :: AST -> Bool
matricesLineUp ast = length (nub (map length matrix)) < 2
  where matrix = astToMatrix ast

rowsMerge :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Bool
rowsMerge a b c d e = mergeRows [[a, c, e], [b, d]] == expected
  where expected = trimEmpty [a ++ b, c ++ d, e]

matricesHaveIds :: Identifier -> AST -> Bool
matricesHaveIds id ast = matrixContains id matrix
  where matrix = astToMatrix (L.List [idToAst id, ast])

matrixContains :: Identifier -> PreMatrix -> Bool
matrixContains id = any (Just (Left (cleanId id)) `elem`)

idToAst id = mkNode [mkNode [mkLeaf "pkg",  mkLeaf (idPackage id)],
                     mkNode [mkLeaf "name", mkLeaf (idName    id)],
                     mkNode [mkLeaf "mod",  mkLeaf (idModule  id)]]

canSubIds = forAll (listOf (do id <- genCleanId
                               n  <- arbitrary
                               return (id, n)))
                   canSubIds'

canSubIds' :: [(Identifier, Int)] -> Property
canSubIds' ids' = forAll (genMatrixWith ids) numbersMatch
  where numbersMatch matrix = all (`elem` map snd ids') (nums matrix)
        f      = fromJust . (`lookup` ids')
        ids    = map (cleanId . fst) ids'
        nums m = [x | row <- subIdsWith f m, Just (Left x) <- row]

canSubStrings :: PreMatrix -> Bool
canSubStrings m = length (subStrings m) == length m

genMatrixWith :: [Identifier] -> Gen PreMatrix
genMatrixWith ids = fmap astToMatrix ast
  where ast = return (mkNode (map (idToAst . cleanId) ids))

genCleanId :: Gen Identifier
genCleanId = fmap cleanId arbitrary

canGetFeatures :: Property
canGetFeatures = forAll (listOf genCleanId >>= genMatrixWith) featuresGotten
  where featuresGotten m = typeCheck (getFeatures f m)
        f id = length (idName id)
        typeCheck :: Features -> Bool
        typeCheck _ = True

canLookupClusters :: [(Identifier, Feature)] -> Bool
canLookupClusters idFeatures = all found idFeatures
  where clusterString = genClusterString idFeatures
        found (id, f) = readClustered clusterString id == f

genClusterString :: [(Identifier, Feature)] -> String
genClusterString = Str.toString . encode . Array . V.fromList . map toObject
  where toObject :: (Identifier, Feature) -> Value
        toObject (id, f) = add (num f) (toJSON id)
        add :: Value -> Value -> Value
        add f (Object o) = Object (HM.insert "cluster" f o)
        num :: Feature -> Value
        num = Number . fromInteger . toInteger

exampleCluster :: Bool
exampleCluster = readClustered arr i1 == 3 &&
                 readClustered arr i2 == 7
  where [o1, o2]   = map mkO [("foo", 3), ("bar", 7)]
        arr        = concat ["[", o1, ", ", o2, "]"]
        mkO (x, c) = concat ["{",
                             "\"package\": \"", x, "Pkg\", ",
                             "\"name\": \"",    x, "Name\", ",
                             "\"module\": \"",  x, "Mod\", ",
                             "\"cluster\": ",   show c,
                             "}"]
        [i1, i2] = map (\x -> ID { idName    = x ++ "Name"
                                 , idModule  = x ++ "Mod"
                                 , idPackage = x ++ "Pkg" })
                       ["foo", "bar"]

defaultFeatureUsed :: Int -> Int -> [[Maybe Int]] -> String -> Bool
defaultFeatureUsed w h m d = outCount >= inCount
  where rows     = renderMatrix' d w h m
        inCount  = length . filter isNothing . concat . fitMatrix w h $ m
        outCount = length . filter (== d)    . concat $ rows

matrixFitsWidth (Positive w) (Positive h) ast = all ((== w) . length) matrix
  where matrix = fitMatrix w h (astToMatrix ast)

matrixFitsHeight (Positive w) (Positive h) ast = length matrix == h
  where matrix = fitMatrix w h (astToMatrix ast)

matrixRenderedToLine (Positive w) (Positive h) ast = commas == w * h - 1
  where commas   = length . filter (== ',') $ rendered
        matrix   = astToMatrix ast
        features = getFeatures (length . idName) matrix
        rendered = renderMatrix "0" w h features

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

exprsGetTrees ctx (EO (e, cs)) = walkTree (toTree cs ctx e)

walkTree (Node x xs) = x >= 0 && all walkTree xs

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
