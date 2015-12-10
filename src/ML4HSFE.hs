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
import GHC.Stack
import           HS2AST.Types

type Matrix a b = [[Maybe (Either a b)]]

type PreMatrix = Matrix Identifier String

type Feature = Int

type Features = [[Maybe Feature]]

data Expr = Var  Id
          | Lit  Literal
          | App  Expr  Expr
          | Lam  Local Expr
          | Let  Bind  Expr
          | Case Expr  Local [Alt]
          | Type

data Id = Local       Local
        | Global      Global
        | Constructor Constructor

data Literal = LitNum
             | LitStr

data Alt = Alt AltCon Expr [Local]

data AltCon = DataAlt Constructor
            | LitAlt  Literal
            | Default

data Bind = NonRec Binder
          | Rec [Binder]

data Binder = Bind Local Expr

newtype Local  = L String
newtype Global = G { unGlobal :: Identifier }

type Constructor = ()

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
exprToMatrix = errorWithStackTrace "exprToMatrix not defined yet"

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
                 Left err -> errorWithStackTrace ("Couldn't read AST: " ++ show err)
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

process :: Int -> Int -> String -> String -> String
process w h rawAst rawDb = let matrix   = astToMatrix (readAst       rawAst)
                               features = getFeatures (readClustered rawDb) matrix
                            in renderMatrix "0" w h features

-- | Our incoming s-expressions are a little awkward. Here we unwrap lists which
--   are nested for no reason, e.g. ((foo bar)) -> (foo bar)
unwrapAst (L.List [L.List xs]) = unwrapAst (L.List xs)
unwrapAst (L.List xs) = L.List (map unwrapAst xs)
unwrapAst x = x

-- | By using generic traversals, '[a, b, c]' becomes '("(:)" a ("(:)" b ("(:)" c)))'
asList (L.List ["(:)", x, xs]) = (x:) <$> asList xs
asList (L.List ["[]"])         = Just []
asList "[]"                    = Just []
asList x                       = errorWithStackTrace ("Not unwrapping list " ++ show x)

asPair (L.List ["(,)", x, y]) = Just (x, y)
asPair x = errorWithStackTrace ("Not unwrapping pair " ++ show x)

asTriple (L.List ["(,,)", x, y, z]) = Just (x, y, z)
asTriple x = errorWithStackTrace ("Not unwrapping triple " ++ show x)

readExpr :: AST -> Maybe Expr
readExpr = readExpr' . unwrapAst

readExpr' ast = case ast of
  -- Happy cases
  L.List ["Lam", l, e]         -> Lam  <$> readLocal l <*> readExpr  e
  L.List ["Lit", x]            -> Lit  <$> readLit   x
  L.List ["App", x, y]         -> App  <$> readExpr' x <*> readExpr' y
  L.List ["Var", x]            -> Var  <$> readId    x
  L.List ["Let", x, y]         -> Let  <$> readBind  x <*> readExpr  y
  L.List ["Case", x, l, _, as] -> do x'   <- readExpr  x
                                     l'   <- readLocal l
                                     as'  <- asList as
                                     as'' <- sequence (map readAlt as')
                                     return (Case x' l' as'')
  L.List ["Type", _]           -> Just Type
  "Type"                       -> Just Type

  -- Things we want to avoid
  L.List ["Coercion", _]              -> Just Type
  L.List ["Tick", _, e]               -> readExpr' e
  L.List ["Cast", e, _]               -> readExpr' e

  _                                   -> errorWithStackTrace ("Unexpected tree " ++ show ast)

readLocal :: AST -> Maybe Local
readLocal (L.List ["Var", x])             = readLocal x
readLocal (L.List (L.List ["name", x]:_)) = readLocal (L.List ["name", x]) -- QuickCheck puts globals in the locals...
readLocal (L.List ["name", L.String x]) = Just (L (S.toString x))

-- FIXME: Parsing s-expressions into Identifiers should be provided by HS2AST
readId x = case x of
  L.List ["Var", e] -> readId e
  L.List ["name", L.String n] -> Just (Local (L (S.toString n)))
  L.List [L.List ["name", L.String n],
          L.List ["mod",  L.String m],
          L.List ["pkg",  L.String p]] -> if isCons (S.toString n)
                                             then Just (Constructor ())
                                             else Just (Global (G (ID {
                                                 idPackage = S.toString p,
                                                 idModule  = S.toString m,
                                                 idName    = S.toString n
                                               })))

-- | Check if a name is that of a constructor
isCons ":"   = True
isCons "[]"  = True
isCons (i:_) = isUpper i
isCons _     = False

readAlt x = do (con, vars, e) <- asTriple x
               con'   <- readAltCon con
               e'     <- readExpr e
               vars'  <- asList vars
               vars'' <- sequence (map readLocal vars')
               return (Alt con' e' vars'')

readAltCon (L.List ["DataAlt", _]) = Just (DataAlt ())
readAltCon (L.List ["LitAlt", l])  = LitAlt <$> readLit l
readAltCon (L.List ["DEFAULT"])    = Just Default
readAltCon x = errorWithStackTrace ("Unexpected AltCon " ++ show x)

readBind b = case b of
  L.List ["Rec", bs]      -> do bs' <- asList bs
                                bs'' <- sequence (map readBinder bs')
                                return (Rec bs'')
  L.List ["NonRec", l, e] -> NonRec <$> (Bind <$> readLocal l <*> readExpr e)

readBinder x = do (l, e) <- asPair x
                  l'     <- readLocal l
                  e'     <- readExpr e
                  return (Bind l' e')

readLit :: AST -> Maybe Literal
readLit (L.List [L.String sort, val]) = case sort of
                                             "MachDouble" -> Just LitNum
                                             "MachInt64"  -> Just LitNum
                                             "MachChar"   -> Just LitStr
                                             "MachStr"    -> Just LitStr
                                             _            -> errorWithStackTrace ("Unexpected literal " ++ S.toString sort)
