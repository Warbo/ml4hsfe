module AllTests (tests) where

import           Data.Aeson
import qualified Data.Graph as DG
import qualified Data.Text as T
import           Grapher
import           HS2AST.Tests.Generators
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import Types

-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
-- divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
-- divideBetween f 0 = return []
-- divideBetween f n = do size <- choose (1, abs n)
--                        head <- f size
--                        tail <- divideBetween f (n - size)
--                        return (head : tail)
-- you can use that to generate a list of recursive calls

geomList :: Gen a -> Gen [a]
geomList g = do x <- g
                oneof [pure [], (x:) <$> geomList g]

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary ASTId where
  arbitrary = do i <- arbitrary
                 d <- geomList arbitrary
                 return ASTId {
                     aId = i
                   , aDeps = d
                   }

instance Arbitrary a => Arbitrary (DG.SCC a) where
  arbitrary = oneof [DG.AcyclicSCC <$> arbitrary,
                     DG.CyclicSCC  <$> listOf1 arbitrary]

tests :: TestTree
tests = testGroup "Tests" [
  QC.testProperty "Decode and encode are inverse"   prop_parse,
  QC.testProperty "Extracting graph preserves size" prop_extract,
  QC.testProperty "Cyclic dependencies are grouped" prop_combinesCycles,
  QC.testProperty "Independent IDs are grouped"     prop_combinesIndependent,
  QC.testProperty "Can get deps of SCCs"            prop_sccDepsMatch,
  QC.testProperty "At least one SCC has no deps"    prop_alwaysDepLessScc,
  QC.testProperty "Group contains all inputs"       prop_bigSCCAll,
  QC.testProperty "Have another SCC"                prop_haveNextSCC,
  QC.testProperty "Combining SCCs preserves length" prop_combineSCCsLength,
  QC.testProperty "SCCs only contain cyclic deps"   prop_sccMinimalCycles]

prop_parse :: ASTId -> Bool
prop_parse x =  (decode . encode $ x)  == Just x

prop_extract :: ASTId -> Bool
prop_extract a = case extractGraphable a of
                      (_, _, t) -> length t == length (aDeps a)

prop_combinesCycles i1 i2 = i1 /= i2 ==>
  case Grapher.group [ASTId { aId = i1, aDeps = [i2] },
                      ASTId { aId = i2, aDeps = [i1] }] of
       [DG.CyclicSCC as] -> let as' = map atomToId as
                             in as' `elem` [[i1, i2], [i2, i1]]
       _                   -> False

prop_combinesIndependent i1 i2 = i1 /= i2 ==>
  case group [ASTId { aId = i1, aDeps = [] },
              ASTId { aId = i2, aDeps = [] }] of
       [DG.CyclicSCC as] -> let as' = map atomToId as
                             in as' `elem` [[i1, i2], [i2, i1]]

prop_sccDepsMatch ids = all (`elem` astDs) sccDs && all (`elem` sccDs) astDs
  where sccDs = concatMap sccDeps (DG.stronglyConnCompR (map extractGraphable ids))
        astDs = filter (not . inIds) (concatMap (map idToAtom . aDeps) ids)
        inIds x = any ((== atomToId x) . aId) ids

prop_alwaysDepLessScc a as = not (null (depLess (DG.stronglyConnCompR xs)))
  where xs = stripUnknownDeps (a:as)

prop_bigSCCAll aids = counterexample debug test
  where test    = all (`elem` grouped) atoms
        grouped = DG.flattenSCCs (group aids)
        atoms   = map (idToAtom . aId) aids
        debug   = show (("grouped", grouped), ("atoms", atoms))

prop_haveNextSCC as = distinct as ==> case nextSCC as of
    Nothing -> null as
    Just x  -> not (null (DG.flattenSCC x))
  where distinct [] = True
        distinct (x:xs) = x `notElem` xs && distinct xs

prop_combineSCCsLength :: [DG.SCC Int] -> Property
prop_combineSCCsLength sccs = not (null sccs) ==> case combineSCCs sccs of
  Just x -> length (DG.flattenSCCs sccs) == length (DG.flattenSCC x)

prop_sccMinimalCycles as' = not (null as) && distinctNames as ==>
    case nextSCC as of
         Nothing -> error ("No SCCs found for " ++ show as)
         Just x  -> let x' = DG.flattenSCC x
                     in all (check x') x'
  where as             = stripUnknownDeps as'
        check xs x     = x `elem` xs && (cyclicOK x xs || acyclicOK x xs)
        cyclicOK  x    = findCycle [] (getFull x)
        acyclicOK x xs = all (`notElem` xs) (depAtoms (getFull x))
        getFull a      = head . filter ((== a) . nameAtom) $ as

-- Helpers

distinctNames []     = True
distinctNames (x:xs) = nameAtom x `notElem` map nameAtom xs && distinctNames xs

findCycle seen x xs = case depAtoms x of
  []     -> False
  (d:ds) -> nameAtom x == d ||
            findCycle (d:seen) x (filter (`notElem` seen) ds ++ xs)
