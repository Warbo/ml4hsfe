module ML4HSFE.Generators where

import Data.Char
import HS2AST.Tests.Generators
import HS2AST.Types
import ML4HSFE.Types
import Test.QuickCheck

instance Arbitrary Global where
  arbitrary = G <$> arbitrary

instance Arbitrary Local where
  arbitrary = L <$> arbitrary

instance Arbitrary Literal where
  arbitrary = elements [LitNum, LitStr]

arbExpr 0 _ = oneof [Lit <$> arbitrary, pure Type]
arbExpr n ctx = oneof [
    Var <$> arbId ctx
  , Lit <$> arbitrary
  , App <$> arbExpr (n `div` 2) ctx <*> arbExpr (n `div` 2) ctx
  , do l <- arbitrary
       Lam l <$> arbExpr (n-1) (l:ctx)
  , do (ls, bs) <- arbBind (n `div` 2) ctx
       Let bs <$> arbExpr (n `div` 2) (ls ++ ctx)
  , do l <- arbitrary
       Case <$> arbExpr (n `div` 2) ctx
            <*> pure l
            <*> divideBetween (\m -> arbAlt m (l:ctx)) (n `div` 2)
  , pure Type
  ]

arbId ctx = oneof ((Global <$> arbitrary) : local)
  where local = if null ctx then []
                            else [Local <$> arbL ctx]

arbBind :: Int -> Context -> Gen (Context, Bind)
arbBind n ctx = oneof [
    do (Bind l x) <- arbBinder n ctx
       return ([l], NonRec (Bind l x))
  , do bs <- divideBetween (`arbBinder` ctx) n
       let ls = map (\(Bind l _) -> l) bs
       return (ls, Rec bs)
  ]

arbBinder :: Int -> Context -> Gen Binder
arbBinder n ctx = Bind <$> arbitrary <*> arbExpr n ctx

arbL :: Context -> Gen Local
arbL = elements

arbAlt n ctx = do ls <- arbitrary
                  Alt <$> arbAC <*> arbExpr n (ls ++ ctx) <*> pure ls

arbAC = oneof [DataAlt <$> arbitrary, LitAlt <$> arbitrary, pure Default]

instance Arbitrary Expr where
  arbitrary = do Positive n <- arbitrary
                 arbExpr n []

safeId = do x <- arbitrary
            return x {
                idName    = safe $ idName x
              , idModule  = safe $ idModule x
              , idPackage = safe $ idPackage x
              }

safe = filter (\x -> isAscii x && isPrint x && x /= '\\')
