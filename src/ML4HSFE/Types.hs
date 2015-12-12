module ML4HSFE.Types where

import HS2AST.Types

data RoseTree = Node Feature [RoseTree] deriving (Show)

type Feature = Either Int Identifier

type Features = [[Maybe Feature]]

data Expr = Var  Id
          | Lit  Literal
          | App  Expr  Expr
          | Lam  Local Expr
          | Let  Bind  Expr
          | Case Expr  Local [Alt]
          | Type
          deriving (Show)

data Id = Local       Local
        | Global      Global
        | Constructor Constructor
        deriving (Show)

data Literal = LitNum
             | LitStr
             deriving (Show)

data Alt = Alt AltCon Expr [Local]  deriving (Show)

data AltCon = DataAlt Constructor
            | LitAlt  Literal
            | Default
            deriving (Show)

data Bind = NonRec Binder
          | Rec [Binder]
          deriving (Show)

data Binder = Bind Local Expr  deriving (Show)

newtype Local  = L String  deriving (Show, Eq)
newtype Global = G { unGlobal :: Identifier }  deriving (Show, Eq)

type Constructor = ()

type Context = [Local]

type Clusters = [[Global]]
