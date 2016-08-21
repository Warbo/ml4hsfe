module Generators where

import CoreSyn
import DataCon
import FastString
import HS2AST.Sexpr
import HS2AST.Types
import HS2AST.Tests.Generators
import Module
import Name
import Packages
import SrcLoc
import Test.QuickCheck
import qualified Data.Stringable as S

sexprWith is = do x <- exprWith is
                  return (toSexp dummyDb x)

exprWith :: [Identifier] -> Gen CoreExpr
exprWith []     = arbitrary
exprWith (i:is) = do tail <- exprWith is
                     sort <- elements ["DataCon"] -- ,"Var", "TyCon"]
                     head <- case sort of
                                  "DataCon" -> do dc <- dataConOf i
                                                  exprUsingDCs [dc]
                     return (App head tail)

nameOf :: Identifier -> (String -> OccName) -> Gen Name
nameOf i mkON = do u <- arbitrary
                   let m = Module {
                               modulePackageKey = stringToPackageKey (S.toString (idPackage i))
                             , moduleName       = mkModuleName       (S.toString (idModule  i))
                             }
                   return (mkExternalName u m (mkON (S.toString (idName i))) noSrcSpan)

-- TODO: Move this to HS2AST's Generators.hs (it's copypasta of arbitrary)
dataConOf i = mkDataCon <$> nameOf i mkDataOcc
                        <*> arbitrary
                        <*> expList arbitrary
                        <*> expList arbitrary
                        <*> expList arbitrary
                        <*> expList arbitrary
                        <*> expList arbitrary
                        <*> arbitrary
                        <*> expList arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

-- TODO: Merge into HS2AST
dummyDb = Just . PackageName . mkFastString . packageKeyString
