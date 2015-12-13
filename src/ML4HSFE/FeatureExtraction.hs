module ML4HSFE.FeatureExtraction where

import Data.List
import HS2AST.Types hiding (Node)
import ML4HSFE.Types

toTree :: Context -> Expr -> RoseTree
toTree ctx x = case x of
  Var i       -> Node fVar  [toTreeI ctx i]
  Lit l       -> Node fLit  [toTreeLit l]
  App e1 e2   -> Node fApp  [toTree ctx e1, toTree ctx e2]
  Lam l e     -> Node fLam  [toTree (l:ctx) e]
  Let b e2    -> let new = localsIn' b
                  in Node fLet  [toTreeBind ctx b, toTree (new ++ ctx) e2]
  Case e l as -> Node fCase (toTree ctx e : map (toTreeAlt (l:ctx)) as)
  Type        -> Node fType []

toTreeI :: Context -> Id -> RoseTree
toTreeI ctx x = case x of
  Local l       -> Node fLocal       [Node (phiL ctx l) []]
  Global g      -> Node fGlobal      [Node (phiG g) []]
  Constructor _ -> Node fConstructor []

toTreeLit x = case x of
  LitNum -> Node fLitNum []
  LitStr -> Node fLitStr []

toTreeAlt :: Context -> Alt -> RoseTree
toTreeAlt ctx x = case x of
  Alt ac e2 ls -> Node fAlt [toTreeAltCon ac, toTree (ls ++ ctx) e2]

toTreeAltCon x = case x of
  DataAlt _ -> Node fDataAlt []
  LitAlt l  -> Node fLitAlt  [toTreeLit l]
  Default   -> Node fDefault []

toTreeBind :: Context -> Bind -> RoseTree
toTreeBind ctx x = case x of
  NonRec b -> let new = localsIn [b]
               in Node fNonRec      [toTreeBinder (new ++ ctx)  b]
  Rec bs   -> let new = localsIn bs
               in Node fRec    (map (toTreeBinder (new ++ ctx)) bs)

-- Extend ctx in toTreeBind rather than here, to allow recursion
toTreeBinder ctx x = case x of
  Bind l e -> Node fBind [toTree ctx e]

-- Look up fresh variable names from buried Binds
localsIn []            = []
localsIn (Bind l _:xs) = l : localsIn xs

localsIn' (NonRec b)  = localsIn [b]
localsIn' (Rec    bs) = localsIn bs

phiL :: Context -> Local -> Feature
phiL ctx x = case elemIndex x ctx of
  Nothing -> error (concat ["Local '", show x, "' not in context '", show ctx, "'"])
  Just i  -> Left $ (2 * alpha) + i

phiG :: Global -> Feature
phiG (G x) = Right x

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

featureVec :: Int -> Int -> Expr -> [Feature]
featureVec r c e = concatMap (\m -> pad (level m tree)) [1..r]
  where tree   = toTree [] e
        pad xs = take c (xs ++ repeat (Left 0))

fixUpE mod pkg names x = recurse x
  where recurse e = case e of
          Var (Local (L n)) | n `elem` names -> Var (Global (G (ID {
                                                  idName    = n,
                                                  idPackage = pkg,
                                                  idModule  = mod })))
          App a b     -> App (recurse a) (recurse b)
          Lam a b     -> Lam a (recurse b)
          Let a b     -> Let (fixUpB mod pkg names a) (recurse b)
          Case a b cs -> Case (recurse a) b (map (fixUpA mod pkg names) cs)
          _           -> e

fixUpB mod pkg names (NonRec (Bind a b)) = NonRec (Bind a (fixUpE mod pkg names b))
fixUpB mod pkg names (Rec xs) = Rec (map (\(Bind a b) -> Bind a (fixUpE mod pkg names b)) xs)

fixUpA mod pkg names (Alt a b cs) = Alt a (fixUpE mod pkg names b) cs
