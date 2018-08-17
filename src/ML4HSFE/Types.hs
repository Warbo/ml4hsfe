{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module ML4HSFE.Types where

import qualified Data.Aeson      as A
import           Control.DeepSeq (($!!), deepseq, force, NFData, rnf)
import qualified Data.Vector     as V
import qualified Data.Text       as T
import           HS2AST.Types

data Entry = Entry {
  entryId        :: !Identifier,
  entryCluster   :: Maybe Int,
  entryToCluster :: !Bool,
  entryFeatures  :: Maybe Features
}

instance A.ToJSON Entry where
  toJSON e = A.object (["name"    A..= idName    id,
                        "module"  A..= idModule  id,
                        "package" A..= idPackage id] ++ c)
    where id = entryId e
          c  = case entryCluster e of
                 Nothing -> []
                 Just n  -> ["cluster" A..= n]

data RoseTree = Node !Feature ![RoseTree] deriving (Show)

type Feature = Either Int Identifier

type Features = V.Vector Feature

data Expr = Var  !Id
          | Lit  !Literal
          | App  !Expr  !Expr
          | Lam  !Local !Expr
          | Let  !Bind  !Expr
          | Case !Expr  !Local ![Alt]
          | Type
          deriving (Show)

instance NFData Expr where
  rnf x = case x of
    Var      i           ->             i `deepseq` ()
    Lit     !l           ->                         ()
    App      f  a        -> f `deepseq` a `deepseq` ()
    Lam  (L !l) e        ->             e `deepseq` ()
    Let      b  e        -> b `deepseq` e `deepseq` ()
    Case     e  (L !l) a -> e `deepseq` a `deepseq` ()
    Type                 ->                         ()

data Id = Local       !Local
        | Global      !Global
        | Constructor !Constructor
        deriving (Show)

instance NFData Id where
  rnf i = case i of
    Local    (L !l) -> ()
    Global   (G !g) -> ()
    Constructor !c  -> ()

data Literal = LitNum
             | LitStr
             deriving (Show)

data Alt = Alt !AltCon !Expr ![Local]  deriving (Show)

instance NFData Alt where
  rnf (Alt ac e l) = let e' = e `deepseq` l'
                         l' = map (\(L !t) -> t) l `deepseq` ()
                      in case ac of
                          Default    -> e'
                          LitAlt  !l -> e'
                          DataAlt !c -> e'

data AltCon = DataAlt !Constructor
            | LitAlt  !Literal
            | Default
            deriving (Show)

data Bind = NonRec !Binder
          | Rec ![Binder]
          deriving (Show)

instance NFData Bind where
  rnf (NonRec !b) = ()
  rnf (Rec    xs) = xs `deepseq` ()

data Binder = Bind !Local !Expr  deriving (Show)

instance NFData Binder where
  rnf (Bind (L !t) !e) = t `deepseq` e `deepseq` ()

newtype Local  = L T.Text                     deriving (Show, Eq)
newtype Global = G { unGlobal :: Identifier } deriving (Show, Eq)

type Constructor = ()

type Context = [Local]
