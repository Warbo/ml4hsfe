{-# LANGUAGE OverloadedStrings #-}

module ML4HSFE where

import Data.AttoLisp as L
import Data.Maybe
import qualified Data.Text as T
import HS2AST.Types

extractIds :: AST -> [Identifier]
extractIds (L.List xs) | have extractPkg xs && have extractName xs && have extractMod xs =
                         let [p] = mapMaybe extractPkg  xs
                             [n] = mapMaybe extractName xs
                             [m] = mapMaybe extractMod  xs
                         in [ID { idName = n, idPackage = p, idModule = m }]
extractIds (L.List xs) = concatMap extractIds xs
extractIds _ = []

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
