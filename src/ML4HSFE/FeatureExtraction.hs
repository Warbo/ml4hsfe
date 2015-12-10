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
  Just i  -> (2 * alpha) + i

phiG :: Clusters -> Global -> Feature
phiG c x = case findIndex (x `elem`) c of
  Nothing -> fRecursion
  Just i  -> i

fRecursion   = 3 * alpha

fAlt         = alpha + 0
fDataAlt     = alpha + 1
fLitAlt      = alpha + 2
fDefault     = alpha + 3
fNonRec      = alpha + 4
fRec         = alpha + 5
fBind        = alpha + 6
fLet         = alpha + 7
fCase        = alpha + 8
fLocal       = alpha + 9
fGlobal      = alpha + 10
fConstructor = alpha + 11
fVar         = alpha + 12
fLam         = alpha + 13
fApp         = alpha + 14
fType        = alpha + 15
fLit         = alpha + 16
fLitNum      = alpha + 17
fLitStr      = alpha + 18

alpha = 100
