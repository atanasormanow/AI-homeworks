module Main (main) where

import Control.Monad (replicateM)
import Data.List (sortBy, (\\))
import Data.Vector hiding (drop, take)
import Immutable.Shuffle (shuffleM)
import System.Random (getStdRandom, randomR)
import Prelude hiding (head, tail)

type VIndividual = Vector Int

type Fitness = Float

type ScoredVIndividual = (VIndividual, Fitness)

type Point = (Int, Int)

type Pair = (VIndividual, VIndividual)

data Selection = Selection [VIndividual] [ScoredVIndividual]
  deriving (Show)

-- -- IO
generatePoints :: Int -> IO (Vector Point)
generatePoints n =
  let genPoint = getStdRandom $ randomR ((-n, -n), (n, n))
   in Data.Vector.replicateM n (genPoint :: IO Point)

permutationOf :: Int -> Int -> IO VIndividual
permutationOf m n = (shuffleM . fromList) [m .. n]

newGeneration :: Int -> Int -> IO [VIndividual]
newGeneration n = flip Control.Monad.replicateM $ permutationOf 0 (n - 1)

-----

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in (sqrt . fromIntegral) (dx * dx + dy * dy)

score :: Vector Point -> VIndividual -> Fitness
score ps inds =
  let op (d, i') i = (d + distance (ps ! i) (ps ! i'), i)
   in fst $ foldl' op (0, head inds) (tail inds)

uninterleave :: [a] -> ([a], [a])
uninterleave = Prelude.foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

-- TODO: add proper implementation
vdiff :: Eq a => Vector a -> Vector a -> Vector a
vdiff v1 v2 =
  let xs = toList v1
      ys = toList v2
   in fromList $ xs \\ ys

-- Generation steps:
--------------------
evalFitness :: Vector Point -> [VIndividual] -> [ScoredVIndividual]
evalFitness ps = Prelude.map (\v -> (v, score ps v))

sortGeneration :: [ScoredVIndividual] -> [ScoredVIndividual]
sortGeneration inds =
  let totalScore = Prelude.foldl (\acc (_, s) -> acc + s) 0 inds
      cmp (_, f1) (_, f2) = compare (f1 / totalScore) (f2 / totalScore)
   in sortBy cmp inds

select :: [ScoredVIndividual] -> Selection
select xs =
  let sorted = sortGeneration xs
      (top70, bot30) = Prelude.splitAt 70 sorted
      new = Prelude.map fst top70
      old = take 20 top70 Prelude.++ drop 20 bot30
   in Selection new old

-- TODO: maybe change types
splitPair :: Pair -> (Pair, IO Int)
splitPair p@(ind1, _) =
  let n = Data.Vector.length ind1
      rIndex = getStdRandom $ randomR (0, n - 1)
   in (p, rIndex)

crossOp :: Int -> Pair -> Pair
crossOp i (parent1, parent2) =
  let (pref1, _) = Data.Vector.splitAt i parent1
      (pref2, _) = Data.Vector.splitAt i parent2
      child1 = pref1 Data.Vector.++ vdiff parent2 pref1
      child2 = pref2 Data.Vector.++ vdiff parent1 pref2
   in (child1, child2)

-- TODO
cross :: [(Pair, IO Int)] -> [Pair]
cross = undefined

swap :: VIndividual -> (Int, Int) -> VIndividual
swap ind (i,j)= ind // [(i, ind ! j), (j, ind ! i)]

-- mutate :: [VIndividual] -> (Vector Int, Vector Int) -> [VIndividual]
-- mutate inds (is, js) = Prelude.zipWith (\ind xy -> swap ind xy)

main :: IO ()
main = do
  gen <- newGeneration 5 10
  points <- generatePoints 5
  let scored = evalFitness points gen
   in print $ sortGeneration scored

-- TODO:
-- - take user input
-- - move some functions in a separate module
-- - since paths containing all nodes are permutations of those nodes
--    then it useless to have a generation with more than n! individuals,
--    where n is the number of nodes (number of genes an individual has)
-- - selection proportions: cross 70to -> 70new; 30new = 20to + 10bo
--    (ill assume i have 100 individuals for now)
-- - mutate using zipWith, permutationOf
