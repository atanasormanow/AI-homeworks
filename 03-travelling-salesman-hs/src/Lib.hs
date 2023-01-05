module Lib
  ( genPoints,
    population,
    mutationSwaps,
    evalFitness,
    sortPopulation,
    evolve,
  )
where

import Control.Monad as Monad (replicateM)
import Data.List (sortBy, (\\))
import Data.Vector as V hiding (concat, foldl, foldr, map, reverse, splitAt, zip, zipWith)
import Immutable.Shuffle (shuffleM)
import System.Random (getStdRandom, randomR)
import Prelude hiding (head, tail)

-- Types
type VIndividual = Vector Int

type Fitness = Float

type ScoredVIndividual = (VIndividual, Fitness)

type Point = (Float, Float)

-- TODO: store all constants used for configuration
-- TODO: record syntax
-- (forBreeding, oldBest, oldWorst)
type Proportions = (Int, Int, Int)

genPoints :: Int -> Float -> IO (Vector Point)
genPoints n range =
  let genPoint = getStdRandom $ randomR ((-range, -range), (range, range))
   in V.replicateM n (genPoint :: IO Point)

permuteFromTo :: Int -> Int -> IO VIndividual
permuteFromTo m n = (shuffleM . fromList) [m .. n]

-- n is the population size
population :: Int -> Int -> IO [VIndividual]
population size n = Monad.replicateM n permutation
  where
    permutation = permuteFromTo 0 (size - 1)

nRandomInRange :: Int -> Int -> Int -> IO [Int]
nRandomInRange n start end =
  let i = getStdRandom $ randomR (start, end)
   in toList <$> V.replicateM n (i :: IO Int)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in sqrt (dx * dx + dy * dy)

-- TODO: maybe overflow here
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
      (top, bot) = splitAt treshold sorted
      new = map fst top
      old = map fst $ Prelude.take best top Prelude.++ Prelude.take worst (reverse bot)
   in (new, old)

-- NOTE: an individual is represented by a permutation
crossOp :: VIndividual -> VIndividual -> [VIndividual]
crossOp parent1 parent2 =
  let i = V.length parent1 `div` 2
      pref1 = V.take i parent1
      pref2 = V.take i parent2
      child1 = pref1 V.++ vdiff parent2 pref1
      child2 = pref2 V.++ vdiff parent1 pref2
   in [child1, child2]

cross :: [VIndividual] -> [VIndividual] -> [VIndividual]
cross ps1 = concat . zipWith crossOp ps1

crossSelected :: ([VIndividual], [VIndividual]) -> [VIndividual]
crossSelected (selected, survivors) =
  let (p1, p2) = uninterleave selected
   in survivors Prelude.++ cross p1 p2

-- Mutation v
mutationSwaps :: Int -> Int -> IO [(Int, Int)]
mutationSwaps n treshold = do
  chances <- nRandomInRange n 0 100
  indexes <- nRandomInRange n 1 (treshold - 2)
  return $ zip chances indexes

-- TODO: avoid out of bounds
-- TODO: take argument instead of using a constant
mutate :: [(Int, Int)] -> [VIndividual] -> [VIndividual]
mutate = zipWith swap
  where
    swap (c, i) ind =
      if c > 10
        then ind
        else ind // [(i - 1, ind ! i + 1), (i + 1, ind ! i - 1)]

---------------------

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
