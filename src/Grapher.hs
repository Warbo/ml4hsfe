{-# LANGUAGE OverloadedStrings #-}
module Grapher where

import           Data.Aeson
import qualified Data.Array as A
import qualified Data.ByteString.Lazy as B
import           Data.Graph
import           Data.Text  (Text)
import           HS2AST.Types
import           Types

type Atom      =  (String, String, String)
type Graphable = [(Atom,   Atom,   [Atom])]

instance Show v => Show (SCC v) where
  show (AcyclicSCC v) = show [v]
  show (CyclicSCC lv) = show lv

toIds :: SCC Atom -> [Identifier]
toIds (AcyclicSCC a)  = [atomToId a]
toIds (CyclicSCC  as) = map atomToId as

renderAll :: [SCC Atom] -> B.ByteString
renderAll = encode . map toIds

-- Handles the case of the Json Key being a Maybe [ASTId]
extractAtoms :: [Identifier] -> [Atom]
extractAtoms = map (\x -> (idName x, idModule x, idPackage x))

-- Turns a Json object into a tuple that's acceptable by graphFromEdges
extractGraphable :: ASTId -> (Atom , Atom, [Atom])
extractGraphable x = let n = aName    x
                         m = aModule  x
                         p = aPackage x
                         d = aDeps    x
                      in ((n,m,p), (n,m,p), extractAtoms d)

parse :: B.ByteString -> [ASTId]
parse s = case eitherDecode s of
               Left err -> error err
               Right ps -> ps

group :: [ASTId] -> [SCC Atom]
group ids = stronglyConnComp atoms
  where atoms = map extractGraphable ids

group' :: [ASTId] -> [SCC (Atom, Atom, [Atom])]
group' = stronglyConnCompR . map extractGraphable

leaves :: [ASTId] -> [ASTId]
leaves = filter (null . aDeps)

-- Dependencies an SCC has on Atoms outside itself
sccDeps :: SCC (Atom, Atom, [Atom]) -> [Atom]
sccDeps s = case s of
    AcyclicSCC a  -> atomDeps a
    CyclicSCC  as -> let inAs x = any (\(y,_,_) -> x == y) as
                      in filter (not . inAs) (concatMap atomDeps as)

atomDeps (_, _, ds) = ds

process :: [ASTId] -> B.ByteString
process = renderAll . group

atomToId (n, m, p) = ID { idPackage = p, idModule = m, idName = n }

idToAtom i = (idName i, idModule i, idPackage i)

atomsToAstId (a, _, ds) = ASTId { aId   = atomToId a,
                                  aDeps = map atomToId ds }
