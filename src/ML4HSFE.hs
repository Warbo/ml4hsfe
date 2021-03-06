{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module ML4HSFE where

import           Data.Aeson                 as Aeson
import           Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BS
import qualified Data.List                  as List
import           Data.Maybe
import qualified Data.Text.Encoding         as TE
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
have f (x:xs) | isNothing (f x) = have f xs
have _ [] = False
have _ _  = True

extractPkg  (L.List [L.String "pkg", L.String p]) = Just p
extractPkg _ = Nothing

extractName (L.List [L.String "name", L.String n]) = Just n
extractName _ = Nothing

extractMod  (L.List [L.String "mod", L.String m]) = Just m
extractMod _ = Nothing

countLeaves (L.String _) = 1
countLeaves (L.List xs)  = sum (map countLeaves xs)

readAst :: BS.ByteString -> AST
readAst s = case AB.eitherResult (AB.parse L.lisp s) of
                 Left err -> error (concat ["Couldn't read AST: ", show err,
                                            "\n", show s])
                 Right x  -> x

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

renderVector :: [Feature] -> BS.ByteString
renderVector = (`BS.snoc` ']') . BS.cons '[' . BS.intercalate "," . map showFeature

showFeature (Left  n) = BS.pack (show n)
showFeature (Right g) = BL.toStrict (Aeson.encode g)

processVal :: Int -> Int -> BS.ByteString -> BS.ByteString -> [BS.ByteString] -> BS.ByteString -> [Feature]
processVal c r mod pkg names rawAst =
  let [mod', pkg'] = map sToL [mod, pkg]
      names'       = map TE.decodeUtf8 names
      ast          = readAst rawAst
      exp          = qualifyExpr mod' pkg' names' ast
      vec          = featureVec c r exp
   in vec

process :: Int             -> Int
        ->  BS.ByteString  -> BS.ByteString
        -> [BS.ByteString] -> BS.ByteString
        ->  BS.ByteString
process c r mod pkg names rawAst = renderVector (processVal c r mod pkg names rawAst)

qualifyAst mod pkg names = qualifyMod mod pkg names . unwrapAst

qualifyExpr mod pkg names = fromJust . readExpr . qualifyAst mod pkg names

sToL = L.String . TE.decodeUtf8
