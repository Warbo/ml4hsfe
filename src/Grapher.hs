{-# LANGUAGE OverloadedStrings #-}
module Grapher where

import           Data.Aeson
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
group = stronglyConnComp . map extractGraphable

process :: [ASTId] -> B.ByteString
process = renderAll . group

atomToId (n, m, p) = ID { idPackage = p, idModule = m, idName = n }
