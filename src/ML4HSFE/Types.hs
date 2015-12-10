module ML4HSFE.Types where

import HS2AST.Types

data RoseTree = Node Feature [RoseTree]

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
