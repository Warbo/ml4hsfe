{-# LANGUAGE OverloadedStrings #-}
module Grapher where

import           Data.Graph
import           Data.Text
import           Jparse


type Atom = (Text, Text, Text)
type Graphable = [(Atom, Atom,[Atom])]

extractAtoms :: Maybe [ASTId] -> [Atom]
extractAtoms (Just (x:xs)) = (name x, modu x, package x) : extractAtoms (pure xs)
extractAtoms _             = []

extractGraphable :: ASTId -> (Atom , Atom, [Atom])
extractGraphable ASTId {
                        name = name,
                        modu = modu,
                        package = package,
                        dependencies = dependencies
                        } = ((name,modu,package), (name,modu,package), extractAtoms dependencies)


dependencySort :: Graphable -> [Atom]
dependencySort x = let (g,v,_) = graphFromEdges x
            in Prelude.map (\x -> let (k,_,_) = v x in k) $ topSort g
