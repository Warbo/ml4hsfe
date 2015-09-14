{-# LANGUAGE OverloadedStrings #-}

module ML4HSFE where

import Data.AttoLisp as L
import Data.Maybe
import qualified Data.Text as T
import HS2AST.Types

type Matrix a b = [[Maybe (Either a b)]]

type PreMatrix = Matrix Identifier String

type Feature = Int

type Features = [[Maybe Feature]]

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
   in Just (ID { idName = n, idPackage = p, idModule = m })
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
astToMatrix = normaliseLengths . astToMatrix'

normaliseLengths :: [[Maybe a]] -> [[Maybe a]]
normaliseLengths xss = map (padToLen (longest xss)) xss

padToLen len xs = take len (xs ++ replicate len Nothing)

countLeaves (L.String _) = 1
countLeaves (L.List xs)  = sum (map countLeaves xs)

longest :: [[a]] -> Int
longest = longest' 0
  where longest' n []     = n
        longest' n (x:xs) = longest' (max n (length x)) xs

astToMatrix' :: AST -> PreMatrix
astToMatrix' (L.String x) = [[Just (Right (T.unpack x))]]
astToMatrix' (L.List xs)  = case extractId (L.List xs) of
                                 Just id -> [[Just (Left id)]]
                                 Nothing -> [] : mergeRows subtrees
  where subtrees = map astToMatrix' xs

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
subIdsWith f rows = map (map switch) rows
  where switch (Just (Left id)) = Just (Left (f id))
        switch (Just (Right x)) = Just (Right x)
        switch Nothing          = Nothing

subStrings :: Matrix a String -> Matrix a Feature
subStrings rows = map (map switch) rows
  where switch (Just (Right x)) = Just (Right (stringFeature x))
        switch (Just (Left  x)) = Just (Left x)
        switch Nothing          = Nothing

-- TODO
stringFeature :: String -> Feature
stringFeature = length

getFeatures :: (Identifier -> Feature) -> PreMatrix -> Features
getFeatures f m = collapse (subStrings (subIdsWith f m))

collapse :: [[Maybe (Either a a)]] -> [[Maybe a]]
collapse rows = map (map f) rows
  where f (Just (Left  x)) = Just x
        f (Just (Right x)) = Just x
        f Nothing = Nothing
