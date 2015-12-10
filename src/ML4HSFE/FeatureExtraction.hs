module ML4HSFE.FeatureExtraction where

import ML4HSFE.Types

toTree x = case x of
  Var i       -> Node fVar  [toTreeI i]
  Lit l       -> Node fLit  [toTreeL l]
  App e1 e2   -> Node fApp  [toTree e1, toTree e2]
  Lam _ e     -> Node fLam  [toTree e]
  Let b e2    -> Node fLet  [toTreeBind b, toTree e2]
  Case e l as -> Node fCase (toTree e : map toTreeAlt as)
  Type        -> Node fType []

toTreeI x = case x of
  Local l       -> Node fLocal       [Node (phiL l) []]
  Global g      -> Node fGlobal      [Node (phiG g) []]
  Constructor _ -> Node fConstructor []

toTreeLit x = case x of
  LitNum -> Node fLitNum []
  LitStr -> Node fLitStr []

toTreeAlt x = case x of
  Alt ac e2 ls -> Node fAlt [toTreeAltCon ac, toTree e2]

toTreeAltCon x = case x of
  DataAlt _ -> Node fDataAlt []
  LitAlt l  -> Node fLitAlt  [toTreeLit l]
  Default   -> Node fDefault []

toTreeBind x = case x of
  NonRec b -> Node fNonRec [toTreeBinder b]
  Rec bs   -> Node fRec    (map toTreeBinder bs)

toTreeBinder x = case x of
  Bind l e -> Node fBind [toTree e]

toTreeL = undefined

phi :: Expr -> Feature
phi = undefined

phiL :: Local -> Feature
phiL = undefined

phiG :: Global -> Feature
phiG = undefined

fVar = undefined

fLit = undefined

fApp = undefined

fLam = undefined

fLet = undefined

fCase = undefined

fType = undefined

fLocal = undefined

fGlobal = undefined

fConstructor = undefined

fLitNum = undefined

fLitStr = undefined

fAlt = undefined

fDataAlt = undefined

fLitAlt = undefined

fDefault = undefined

fNonRec = undefined

fRec = undefined

fBind = undefined
