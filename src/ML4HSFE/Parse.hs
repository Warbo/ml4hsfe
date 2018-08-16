{-# LANGUAGE OverloadedStrings #-}
module ML4HSFE.Parse where

import qualified Control.DeepSeq as DS
import qualified Data.AttoLisp   as L
import           Data.Char
import qualified Data.Text       as T
import           GHC.Stack
import           HS2AST.Types
import           ML4HSFE.Types

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

qualifyMod mod pkg names x = case x of
  L.List ["Var", L.List ["name", L.String n]] | n `elem` names ->
    L.List ["Var", L.List [L.List ["name", L.String n], L.List ["mod", mod], L.List ["pkg", pkg]]]
  L.List xs -> L.List (map (qualifyMod mod pkg names) xs)
  _ -> x

readExpr :: AST -> Maybe Expr
readExpr = DS.force . readExpr' . unwrapAst

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
                                     as'' <- mapM readAlt as'
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
readLocal (L.List ["name", L.String x]) = Just (L x)
readLocal x = errorWithStackTrace ("Unexpected argument to readLocal " ++ show x)

-- FIXME: Parsing s-expressions into Identifiers should be provided by HS2AST
readId x = case x of
  L.List ["Var", e] -> readId e
  L.List ["name", L.String n] -> Just (Local (L n))
  L.List [L.List ["name", L.String n],
          L.List ["mod",  L.String m],
          L.List ["pkg",  L.String p]] -> if isCons n
                                             then Just (Constructor ())
                                             else Just (Global (G ID {
                                                 idPackage = p,
                                                 idModule  = m,
                                                 idName    = n
                                               }))
  _ -> errorWithStackTrace ("Unexpected argument to readId " ++ show x)

-- | Check if a name is that of a constructor
isCons ":"  = True
isCons "[]" = True
isCons ""   = False
isCons x    = isUpper (T.head x)

readAlt x = do (con, vars, e) <- asTriple x
               con'   <- readAltCon con
               e'     <- readExpr e
               vars'  <- asList vars
               vars'' <- mapM readLocal vars'
               return (Alt con' e' vars'')

readAltCon (L.List ["DataAlt", _]) = Just (DataAlt ())
readAltCon (L.List ["LitAlt", l])  = LitAlt <$> readLit l
readAltCon (L.List ["DEFAULT"])    = Just Default
readAltCon x = errorWithStackTrace ("Unexpected AltCon " ++ show x)

readBind b = case b of
  L.List ["Rec", bs]      -> do bs' <- asList bs
                                bs'' <- mapM readBinder bs'
                                return (Rec bs'')
  L.List ["NonRec", l, e] -> NonRec <$> (Bind <$> readLocal l <*> readExpr e)
  _ -> errorWithStackTrace ("Unexpected argument to readBind " ++ show b)

readBinder x = do (l, e) <- asPair x
                  l'     <- readLocal l
                  e'     <- readExpr e
                  return (Bind l' e')

readLit :: AST -> Maybe Literal
readLit (L.List (L.String sort:_)) = case sort of
                                             "MachChar"     -> Just LitStr
                                             "MachStr"      -> Just LitStr
                                             "MachNullAddr" -> Just LitNum
                                             "MachInt"      -> Just LitNum
                                             "MachInt64"    -> Just LitNum
                                             "MachWord"     -> Just LitNum
                                             "MachWord64"   -> Just LitNum
                                             "MachFloat"    -> Just LitNum
                                             "MachDouble"   -> Just LitNum
                                             "MachLabel"    -> Just LitStr
                                             "LitInteger"   -> Just LitNum
                                             _              -> Just LitNum
readLit x = errorWithStackTrace ("Unexpected readLit argument " ++ show x)
