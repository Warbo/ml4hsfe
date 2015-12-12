module ML4HSFE.FeatureExtraction where

import Data.List
import ML4HSFE.Types

toTree :: Clusters -> Context -> Expr -> RoseTree
toTree cs ctx x = case x of
  Var i       -> Node fVar  [toTreeI cs ctx i]
  Lit l       -> Node fLit  [toTreeLit l]
  App e1 e2   -> Node fApp  [toTree cs ctx e1, toTree cs ctx e2]
  Lam l e     -> Node fLam  [toTree cs (l:ctx) e]
  Let b e2    -> let new = localsIn' b
                  in Node fLet  [toTreeBind cs ctx b, toTree cs (new ++ ctx) e2]
  Case e l as -> Node fCase (toTree cs ctx e : map (toTreeAlt cs (l:ctx)) as)
  Type        -> Node fType []

toTreeI :: Clusters -> Context -> Id -> RoseTree
toTreeI cs ctx x = case x of
  Local l       -> Node fLocal       [Node (phiL ctx l) []]
  Global g      -> Node fGlobal      [Node (phiG cs  g) []]
  Constructor _ -> Node fConstructor []

toTreeLit x = case x of
  LitNum -> Node fLitNum []
  LitStr -> Node fLitStr []

toTreeAlt :: Clusters -> Context -> Alt -> RoseTree
toTreeAlt cs ctx x = case x of
  Alt ac e2 ls -> Node fAlt [toTreeAltCon ac, toTree cs (ls ++ ctx) e2]

toTreeAltCon x = case x of
  DataAlt _ -> Node fDataAlt []
  LitAlt l  -> Node fLitAlt  [toTreeLit l]
  Default   -> Node fDefault []

toTreeBind :: Clusters -> Context -> Bind -> RoseTree
toTreeBind cs ctx x = case x of
  NonRec b -> let new = localsIn [b]
               in Node fNonRec      [toTreeBinder cs (new ++ ctx)  b]
  Rec bs   -> let new = localsIn bs
               in Node fRec    (map (toTreeBinder cs (new ++ ctx)) bs)

-- Extend ctx in toTreeBind rather than here, to allow recursion
toTreeBinder cs ctx x = case x of
  Bind l e -> Node fBind [toTree cs ctx e]

-- Look up fresh variable names from buried Binds
localsIn []            = []
localsIn (Bind l _:xs) = l : localsIn xs

localsIn' (NonRec b)  = localsIn [b]
localsIn' (Rec    bs) = localsIn bs

phiL :: Context -> Local -> Feature
phiL ctx x = case elemIndex x ctx of
  Nothing -> error (concat ["Local '", show x, "' not in context '", show ctx, "'"])
  Just i  -> Left $ (2 * alpha) + i

phiG :: Clusters -> Global -> Feature
phiG cs (G x) = Right x

fRecursion   = Left $ 3 * alpha

fAlt         = Left $ alpha + 0
fDataAlt     = Left $ alpha + 1
fLitAlt      = Left $ alpha + 2
fDefault     = Left $ alpha + 3
fNonRec      = Left $ alpha + 4
fRec         = Left $ alpha + 5
fBind        = Left $ alpha + 6
fLet         = Left $ alpha + 7
fCase        = Left $ alpha + 8
fLocal       = Left $ alpha + 9
fGlobal      = Left $ alpha + 10
fConstructor = Left $ alpha + 11
fVar         = Left $ alpha + 12
fLam         = Left $ alpha + 13
fApp         = Left $ alpha + 14
fType        = Left $ alpha + 15
fLit         = Left $ alpha + 16
fLitNum      = Left $ alpha + 17
fLitStr      = Left $ alpha + 18

alpha = 100

level :: Int -> RoseTree -> [Feature]
level 1 (Node f _)  = [f]
level n (Node _ ts) = concatMap (level (n-1)) ts

featureVec :: Int -> Int -> Clusters -> Expr -> [Feature]
featureVec r c cs e = concatMap (\m -> pad (level m tree)) [1..r]
  where tree   = toTree cs [] e
        pad xs = take c (xs ++ repeat (Left 0))