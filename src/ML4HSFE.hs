{-# LANGUAGE OverloadedStrings #-}

module ML4HSFE where

import           Data.Aeson                 as Aeson
import           Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import           Data.Char
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as List
import           Data.Maybe
import qualified Data.Scientific            as Sci
import qualified Data.Stringable            as S
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           ML4HSFE.FeatureExtraction
import           HS2AST.Types
import           ML4HSFE.Parse
import           ML4HSFE.Types

extractIds :: AST -> [Identifier]
extractIds (L.List xs) = case extractId (L.List xs) of
  Just x  -> x : concatMap extractIds xs
  Nothing ->     concatMap extractIds xs
extractIds _ = []

extractId :: AST -> Maybe Identifier
extractId (L.List xs) | have extractPkg  xs &&
                        have extractName xs &&
                        have extractMod  xs =
  let [p] = mapMaybe extractPkg  xs
      [n] = mapMaybe extractName xs
      [m] = mapMaybe extractMod  xs
   in Just ID { idName = n, idPackage = p, idModule = m }
extractId _ = Nothing

have :: (a -> Maybe b) -> [a] -> Bool
have f [] = False
have f (x:xs) | isNothing (f x) = have f xs
have _ _ = True

extractPkg (L.List [L.String "pkg", L.String p]) = Just (T.unpack p)
extractPkg _ = Nothing

extractName (L.List [L.String "name", L.String n]) = Just (T.unpack n)
extractName _ = Nothing

extractMod (L.List [L.String "mod", L.String m]) = Just (T.unpack m)
extractMod _ = Nothing

astToMatrix :: AST -> PreMatrix
astToMatrix ast = normaliseLengths (maybe [] exprToMatrix (readExpr ast))

normaliseLengths :: [[Maybe a]] -> [[Maybe a]]
normaliseLengths xss = map (padToLen (longest xss)) xss

padToLen len xs = take len (xs ++ replicate len Nothing)

countLeaves (L.String _) = 1
countLeaves (L.List xs)  = sum (map countLeaves xs)

longest :: [[a]] -> Int
longest = longest' 0
  where longest' = foldl (\n x -> max n (length x))

exprToMatrix :: Expr -> PreMatrix
exprToMatrix = error "exprToMatrix not defined yet"

mergeRows :: [[[a]]] -> [[a]]
mergeRows = trimEmpty . mergeRows'

trimEmpty = reverse . dropWhile null . reverse

mergeRows' [] = []
mergeRows' xs = mconcat heads : mergeRows' tails
  where heads = mapMaybe safeHead xs
        tails = mapMaybe safeTail xs

safeHead (x:xs) = Just x
safeHead []     = Nothing

safeTail (x:xs) = Just xs
safeTail []     = Nothing

subIdsWith :: (Identifier -> a)
           -> Matrix Identifier b
           -> Matrix a b
subIdsWith f = map (map switch)
  where switch (Just (Left id)) = Just (Left (f id))
        switch (Just (Right x)) = Just (Right x)
        switch Nothing          = Nothing

subStrings :: Matrix a String -> Matrix a Feature
subStrings = map (map switch)
  where switch (Just (Right x)) = Just (Right (stringFeature x))
        switch (Just (Left  x)) = Just (Left x)
        switch Nothing          = Nothing

-- TODO
stringFeature :: String -> Feature
stringFeature = length

getFeatures :: (Identifier -> Feature) -> PreMatrix -> Features
getFeatures f m = collapse (subStrings (subIdsWith f m))

collapse :: [[Maybe (Either a a)]] -> [[Maybe a]]
collapse = map (map f)
  where f (Just (Left  x)) = Just x
        f (Just (Right x)) = Just x
        f Nothing = Nothing

readAst :: String -> AST
readAst s = case AB.eitherResult (AB.parse L.lisp (S.fromString s)) of
                 Left err -> error ("Couldn't read AST: " ++ show err)
                 Right x  -> x

readClustered :: String -> Identifier -> Feature
readClustered s id = fromMaybe dEFAULT (lookup id . clustersFrom $ s)

dEFAULT = 0

clustersFrom :: String -> [(Identifier, Feature)]
clustersFrom s = map valToPair vals
  where Just (Array arr) = decode . S.fromString $ s
        vals      = V.toList arr
        valToPair (Object o) = let Just (Aeson.Number f) = HM.lookup "cluster" o
                                   rawId   = Object (HM.delete "cluster" o)
                                   Just id = Aeson.decode . Aeson.encode $ rawId
                                   Just f' = Sci.toBoundedInteger f
                                in (id, f')

data WithFeature = WC {
    wcId      :: Identifier
  , wcFeature :: Feature
  }

instance FromJSON WithFeature where
  parseJSON (Object x) = do
    p <- x .: "package"
    m <- x .: "module"
    n <- x .: "name"
    f <- x .: "cluster"
    return WC { wcId = ID { idPackage = p, idModule = m, idName = n },
                wcFeature = f }

renderMatrix :: (Show a) => String -> Int -> Int -> [[Maybe a]] -> String
renderMatrix def w h = List.intercalate "," . concat . renderMatrix' def w h

renderMatrix' :: (Show a) => String -> Int -> Int -> [[Maybe a]] -> [[String]]
renderMatrix' def w h = map (map f) . fitMatrix w h
  where f Nothing  = def
        f (Just x) = show x

fitMatrix :: Int -> Int -> [[Maybe a]] -> [[Maybe a]]
fitMatrix width height m = padTo height empty (map (padTo width Nothing) m)
  where padTo n x xs = take n (xs ++ replicate n x)
        empty        = replicate width Nothing

renderVector :: [Feature] -> String
renderVector = List.intercalate "," . map show

clustersFrom' x = collate [] (clustersFrom x)
  where collate cs [] = cs
        collate cs ((x, i):xs) = collate (insert x i cs) xs
        insert x i cs | i < 1 = error ("Can't insert at index " ++ show i)
        insert x 1 (y:ys) = ((x:y):ys)
        insert x i (y:ys) = y : insert x (i-1) ys
        insert x i []     = insert x i [[]]

process :: Int -> Int -> String -> String -> String
process c r rawAst rawDb = let ast = readAst rawAst
                               cs  = map (map G) (clustersFrom' rawDb)
                               Just exp = readExpr ast
                               vec = featureVec c r cs exp
                            in renderVector vec
