{-# LANGUAGE OverloadedStrings #-}
module Grapher where

import           Data.Graph
import           Data.Text
import           Jparse


type Atom = (Text, Text, Text)
type Graphable = [(Atom, Atom,[Atom])]


instance Show v => Show (SCC v) where
  show (AcyclicSCC v) = show [v]
  show (CyclicSCC lv) = show lv

-- Handles the case of the Json Key being a Maybe [ASTId]
extractAtoms :: Maybe [ASTId] -> [Atom]
extractAtoms (Just (x:xs)) = (name x, modu x, package x) : extractAtoms (pure xs)
extractAtoms _             = []


-- Turns a Json object into a tuple that's acceptable by graphFromEdges
extractGraphable :: ASTId -> (Atom , Atom, [Atom])
extractGraphable ASTId {
                        name = name,
                        modu = modu,
                        package = package,
                        dependencies = dependencies
                        } = ((name,modu,package), (name,modu,package), extractAtoms dependencies)


-- graphFromEdges :: Ord key => [(Atom, Atom, [Atom])]
--                              -> (Graph, Vertex -> (Atom, Atom, [Atom]), Atom -> Maybe Vertex)
-- dependencySort :: Graphable -> [Atom]
-- dependencySort [] = []
-- dependencySort x = let (g,v,_) = graphFromEdges x
--             in Prelude.map (\x -> let (k,_,_) = v x in k) $ topSort g
