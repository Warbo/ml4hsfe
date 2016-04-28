{-# LANGUAGE OverloadedStrings #-}
module Helpers where

import qualified Data.AttoLisp       as L
import           Data.Char
import qualified Data.Stringable     as Str
import           Generators
import           HS2AST.Sexpr
import           HS2AST.Types hiding (Node)
import           ML4HSFE             as FE
import           ML4HSFE.Parse
import           Test.QuickCheck

mkLisp :: Int -> L.Lisp -> Gen L.Lisp
mkLisp n l = case n of
  0 -> pure l
  _ | n `mod` 3 == 0 -> do x <- arbitrary `suchThat` validLocal
                           mkLisp (n-1) (L.List ["Lam", L.List ["Var", L.List ["name", x]], l])
  _ | n `mod` 5 == 0 -> do x <- arbitrary
                           mkLisp (n-1) (L.List ["App", l, L.List ["Lit", L.List ["MachStr", x]]])
  _                  -> do x <- arbitrary `suchThat` validLocal
                           mkLisp (n-1) (L.List ["App", L.List ["Lam", L.List ["Var", L.List ["name", x]],
                                                                       L.List ["Var", L.List ["name", x]]],
                                                        l])

validLocal (L.String x) = validText x
validLocal _ = False

validText = validName . Str.toString

validName s = not (null s) && all isPrint s && all isAscii s && all isAlphaNum s

validId i = validName (idName i) && validName (idPackage i) && validName (idModule i)

validVar x = validName x && not (isCons x)

mkId = do n <- arbitrary `suchThat` validVar
          m <- arbitrary `suchThat` validName
          p <- arbitrary `suchThat` validName
          return ID { idName = n, idModule = m, idPackage = p}

asLocal i = L.List ["Var", L.List ["name", L.String (Str.fromString (idName i))]]

asGlobal i = L.List ["Var", L.List [L.List ["name", sToL (idName    i)],
                                    L.List ["mod",  sToL (idModule  i)],
                                    L.List ["pkg",  sToL (idPackage i)]]]