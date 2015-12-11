module ML4HSFE.Generators where

import HS2AST.Tests.Generators
import ML4HSFE.Types
import Test.QuickCheck

instance Arbitrary Global where
  arbitrary = G <$> arbitrary

instance Arbitrary Local where
  arbitrary = L <$> arbitrary

instance Arbitrary Literal where
  arbitrary = elements [LitNum, LitStr]

arbExpr 0 _ _ = oneof [Lit <$> arbitrary, pure Type]
arbExpr n cs ctx = oneof [
    Var <$> arbId cs ctx
  , Lit <$> arbitrary
  , App <$> arbExpr (n `div` 2) cs ctx <*> arbExpr (n `div` 2) cs ctx
  , do l <- arbitrary
       Lam l <$> arbExpr (n-1) cs (l:ctx)
  , do (ls, bs) <- arbBind (n `div` 2) cs ctx
       Let bs <$> arbExpr (n `div` 2) cs (ls ++ ctx)
  , do l <- arbitrary
       Case <$> arbExpr (n `div` 2) cs ctx
            <*> pure l
            <*> divideBetween (\m -> arbAlt m cs (l:ctx)) (n `div` 2)
  , pure Type
  ]

arbId cs ctx = oneof ([Global <$> arbitrary] ++ local ++ global)
  where local  = if null ctx then []
                             else [Local <$> arbL ctx]
        global = if null (concat cs) then []
                                     else map (pure . Global) (concat cs)

arbBind :: Int -> Clusters -> Context -> Gen (Context, Bind)
arbBind n cs ctx = oneof [
    do (Bind l x) <- arbBinder n cs ctx
       return ([l], NonRec (Bind l x))
  , do bs <- divideBetween (\m -> arbBinder m cs ctx) n
       let ls = map (\(Bind l _) -> l) bs
       return (ls, Rec bs)
  ]

arbBinder :: Int -> Clusters -> Context -> Gen Binder
arbBinder n cs ctx = Bind <$> arbitrary <*> arbExpr n cs ctx

arbL :: Context -> Gen Local
arbL = elements

arbAlt n cs ctx = do ls <- arbitrary
                     Alt <$> arbAC <*> arbExpr n cs (ls ++ ctx) <*> pure ls

arbAC = oneof [DataAlt <$> arbitrary, LitAlt <$> arbitrary, pure Default]

newtype ExprOf = EO (Expr, Clusters) deriving (Show)

instance Arbitrary ExprOf where
  arbitrary = do cs <- arbitrary
                 Positive n <- arbitrary
                 e <- arbExpr n cs []
                 return (EO (e, cs))
