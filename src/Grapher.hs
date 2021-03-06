{-# LANGUAGE BangPatterns #-}
module Grapher where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Graph
import qualified Data.Text  as T
import           HS2AST.Types
import           Types

type Atom      =  (T.Text, T.Text, T.Text)
type Graphable = [(Atom,   Atom,   [Atom])]

instance Show v => Show (SCC v) where
  show (AcyclicSCC v) = show [v]
  show (CyclicSCC lv) = show lv

toIds :: SCC Atom -> [Identifier]
toIds (AcyclicSCC a)  = [atomToId a]
toIds (CyclicSCC  as) = map atomToId as

renderAll :: [SCC Atom] -> B.ByteString
renderAll !l = let !bs = encode (map toIds l)
                in bs

-- Handles the case of the Json Key being a Maybe [ASTId]
extractAtoms :: [Identifier] -> [Atom]
extractAtoms !l = map (\x -> let !n = idName    x
                                 !m = idModule  x
                                 !p = idPackage x
                              in (n, m, p))
                      l

-- Turns a Json object into a tuple that's acceptable by graphFromEdges
extractGraphable :: ASTId -> (Atom , Atom, [Atom])
extractGraphable x = let !n = aName    x
                         !m = aModule  x
                         !p = aPackage x
                         !d = aDeps    x
                         !a = extractAtoms d
                      in ((n,m,p), (n,m,p), a)

injectGraphable :: (Atom, Atom, [Atom]) -> ASTId
injectGraphable (a, _, as) = ASTId { aId   = atomToId a,
                                     aDeps = map atomToId as }

-- Includes as many Atoms as possible in each group
group :: [ASTId] -> [SCC Atom]
group = bigSCCs . map extractGraphable

bigSCCs :: [(Atom, Atom, [Atom])] -> [SCC Atom]
bigSCCs !as = case nextSCC as of
  Nothing -> []
  Just !y -> y : bigSCCs (removeAtoms (sccAtoms y) as)

sccAtoms :: SCC Atom -> [Atom]
sccAtoms = flattenSCC

-- Dependencies an SCC has on Atoms outside itself
sccDeps :: SCC (Atom, Atom, [Atom]) -> [Atom]
sccDeps s = case s of
    AcyclicSCC a  -> depAtoms a
    CyclicSCC  as -> let inAs x = any (\(y,_,_) -> x == y) as
                      in filter (not . inAs) (concatMap depAtoms as)

depLess :: [SCC (Atom, Atom, [Atom])] -> [SCC (Atom, Atom, [Atom])]
depLess = filter (null . sccDeps)

-- Combine as many elements as we can into one SCC
nextSCC :: [(Atom, Atom, [Atom])] -> Maybe (SCC Atom)
nextSCC [] = Nothing
nextSCC as = forgetAtoms <$> combineSCCs sccs
  where sccs = depLess $! (stronglyConnCompR $! stripUnknownDeps as)

combineSCCs :: [SCC a] -> Maybe (SCC a)
combineSCCs ss = case ss of
  []     -> Nothing
  [x]    -> Just x
  (x:ys) -> case (x, combineSCCs ys) of
    (_,            Nothing)             -> Just x
    (AcyclicSCC a, Just (AcyclicSCC b)) -> Just (CyclicSCC [a, b])
    (AcyclicSCC a, Just (CyclicSCC bs)) -> Just (CyclicSCC (a:bs))
    (CyclicSCC as, Just (AcyclicSCC b)) -> Just (CyclicSCC (b:as))
    (CyclicSCC as, Just (CyclicSCC bs)) -> Just (CyclicSCC (as ++ bs))

forgetAtoms :: SCC (Atom, Atom, [Atom]) -> SCC Atom
forgetAtoms (AcyclicSCC a) = AcyclicSCC (nameAtom a)
forgetAtoms (CyclicSCC as) = CyclicSCC  (map nameAtom as)

stripUnknownDeps :: [(Atom, Atom, [Atom])] -> [(Atom, Atom, [Atom])]
stripUnknownDeps as = map strip as
  where strip (x, y, zs) = (x, y, filter (`elem` known) zs)
        known = map nameAtom as

removeAtoms :: [Atom] -> [(Atom, Atom, [Atom])] -> [(Atom, Atom, [Atom])]
removeAtoms as = stripUnknownDeps . filter (not . (`elem` as) . nameAtom)

nameAtom :: (Atom, Atom, [Atom]) -> Atom
nameAtom (n, _, _)  = n

depAtoms :: (Atom, Atom, [Atom]) -> [Atom]
depAtoms (_, _, ds) = ds

process :: [ASTId] -> B.ByteString
process !l = renderAll $! group l

atomToId (n, m, p) = ID { idPackage = p, idModule = m, idName = n }

idToAtom i = (idName i, idModule i, idPackage i)

atomsToAstId (a, _, ds) = ASTId { aId   = atomToId a,
                                  aDeps = map atomToId ds }
