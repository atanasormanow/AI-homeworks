module Lib
  ( genPoints,
    population,
    mutationSwaps,
    evalFitness,
    sortPopulation,
    evolve,
  )
where

import Control.Monad (replicateM)
import Data.List (sortBy, (\\))
import Data.Vector hiding (concat, drop, foldl, foldr, map, reverse, take, zip, zipWith, zipWith3)
import Immutable.Shuffle (shuffleM)
import System.Random (getStdRandom, randomR)
import Prelude hiding (head, tail)

-- Types
type VIndividual = Vector Int

type Fitness = Float

type ScoredVIndividual = (VIndividual, Fitness)

type Point = (Float, Float)

-- (forBreeding, oldBest, oldWorst)
type Proportions = (Int, Int, Int)

-- Impure functions:
--------------------

-- n is used in 2 different contexts
genPoints :: Int -> Float -> IO (Vector Point)
genPoints n range =
  let genPoint = getStdRandom $ randomR ((-range, -range), (range, range))
   in Data.Vector.replicateM n (genPoint :: IO Point)

permuteFromTo :: Int -> Int -> IO VIndividual
permuteFromTo m n = (shuffleM . fromList) [m .. n]

population :: Int -> Int -> IO [VIndividual]
population n = flip Control.Monad.replicateM $ permuteFromTo 0 (n - 1)

randomIndexes :: Int -> Int -> IO [Int]
randomIndexes n treshold =
  let i = getStdRandom $ randomR (0, treshold)
   in toList <$> Data.Vector.replicateM n (i :: IO Int)

mutationSwaps :: Int -> Int -> IO [(Int, Int)]
mutationSwaps n treshold = do
  p1 <- randomIndexes n (treshold - 1)
  p2 <- randomIndexes n (treshold - 1)
  return $ zip p1 p2

--------------------

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in sqrt (dx * dx + dy * dy)

score :: Vector Point -> VIndividual -> Fitness
score ps inds =
  let op (d, i') i = (d + distance (ps ! i) (ps ! i'), i)
   in fst $ foldl' op (0, head inds) (tail inds)

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

-- TODO: add proper implementation
vdiff :: Eq a => Vector a -> Vector a -> Vector a
vdiff v1 v2 =
  let xs = toList v1
      ys = toList v2
   in fromList $ xs \\ ys

evalFitness :: Vector Point -> [VIndividual] -> [ScoredVIndividual]
evalFitness ps = map (\v -> (v, score ps v))

sortPopulation :: [ScoredVIndividual] -> [ScoredVIndividual]
sortPopulation inds =
  let totalScore = foldl (\acc (_, s) -> acc + s) 0 inds
      cmp (_, f1) (_, f2) = compare (f1 / totalScore) (f2 / totalScore)
   in sortBy cmp inds

select :: Proportions -> [ScoredVIndividual] -> ([VIndividual], [VIndividual])
select (treshold, best, worst) xs =
  let sorted = sortPopulation xs
      (top, bot) = Prelude.splitAt treshold sorted
      new = map fst top
      old = map fst $ take best top Prelude.++ take worst (reverse bot)
   in (new, old)

crossOp :: VIndividual -> VIndividual -> [VIndividual]
crossOp parent1 parent2 =
  let i = Data.Vector.length parent1 `div` 2
      (pref1, _) = Data.Vector.splitAt i parent1
      (pref2, _) = Data.Vector.splitAt i parent2
      child1 = pref1 Data.Vector.++ vdiff parent2 pref1
      child2 = pref2 Data.Vector.++ vdiff parent1 pref2
   in [child1, child2]

cross :: [VIndividual] -> [VIndividual] -> [VIndividual]
cross ps1 = concat . zipWith crossOp ps1

crossSelected :: ([VIndividual], [VIndividual]) -> [VIndividual]
crossSelected (selected, survivors) =
  let (p1, p2) = uninterleave selected
   in survivors Prelude.++ cross p1 p2

swap :: (Int, Int) -> VIndividual -> VIndividual
swap (i, j) ind = ind // [(i, ind ! j), (j, ind ! i)]

mutate :: [(Int, Int)] -> [VIndividual] -> [VIndividual]
mutate = zipWith swap

evolve ::
  Vector Point ->
  Proportions ->
  [(Int, Int)] ->
  [VIndividual] ->
  [VIndividual]
evolve points proportions mutations =
  mutate mutations
    . crossSelected
    . select proportions
    . sortPopulation
    . evalFitness points
