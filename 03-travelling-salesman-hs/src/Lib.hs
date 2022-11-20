module Lib
  ( genPoints,
    population,
    mutationSwaps,
    evolve,
    test,
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

type Point = (Int, Int)

-- (forBreeding, OldBest, OldWorst)
type Proportions = (Int, Int, Int)

genPoints :: Int -> IO (Vector Point)
genPoints n =
  let genPoint = getStdRandom $ randomR ((-n, -n), (n, n))
   in Data.Vector.replicateM n (genPoint :: IO Point)

permuteFromTo :: Int -> Int -> IO VIndividual
permuteFromTo m n = (shuffleM . fromList) [m .. n]

population :: Int -> Int -> IO [VIndividual]
population n = flip Control.Monad.replicateM $ permuteFromTo 0 (n - 1)

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
uninterleave = foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

-- TODO: add proper implementation
vdiff :: Eq a => Vector a -> Vector a -> Vector a
vdiff v1 v2 =
  let xs = toList v1
      ys = toList v2
   in fromList $ xs \\ ys

evalFitness :: Vector Point -> [VIndividual] -> [ScoredVIndividual]
evalFitness ps = map (\v -> (v, score ps v))

sortGeneration :: [ScoredVIndividual] -> [ScoredVIndividual]
sortGeneration inds =
  let totalScore = foldl (\acc (_, s) -> acc + s) 0 inds
      cmp (_, f1) (_, f2) = compare (f1 / totalScore) (f2 / totalScore)
   in sortBy cmp inds

select :: Proportions -> [ScoredVIndividual] -> ([VIndividual], [VIndividual])
select (treshold, best, worst) xs =
  let sorted = sortGeneration xs
      (top, bot) = Prelude.splitAt treshold sorted
      new = map fst top
      old = map fst $ take best top Prelude.++ take worst (reverse bot)
   in (new, old)

crossOp :: Int -> VIndividual -> VIndividual -> [VIndividual]
crossOp i parent1 parent2 =
  let (pref1, _) = Data.Vector.splitAt i parent1
      (pref2, _) = Data.Vector.splitAt i parent2
      child1 = pref1 Data.Vector.++ vdiff parent2 pref1
      child2 = pref2 Data.Vector.++ vdiff parent1 pref2
   in [child1, child2]

cross :: [Int] -> [VIndividual] -> [VIndividual] -> [VIndividual]
cross ps1 ps2 = concat . zipWith3 crossOp ps1 ps2

crossSelected :: [Int] -> ([VIndividual], [VIndividual]) -> [VIndividual]
crossSelected indexes (selected, survivors) =
  let (p1, p2) = uninterleave selected
   in survivors Prelude.++ cross indexes p1 p2

swap :: (Int, Int) -> VIndividual -> VIndividual
swap (i, j) ind = ind // [(i, ind ! j), (j, ind ! i)]

mutate :: [(Int, Int)] -> [VIndividual] -> [VIndividual]
mutate = zipWith swap

evolve ::
  Vector Point ->
  Proportions ->
  [Int] ->
  [(Int, Int)] ->
  [VIndividual] ->
  [VIndividual]
evolve points proportions crossIndexes mutations =
  mutate mutations
    . crossSelected crossIndexes
    . select proportions
    . sortGeneration
    . evalFitness points

randomIndexes :: Int -> Int -> IO [Int]
randomIndexes n treshold =
  let i = getStdRandom $ randomR (0, treshold)
   in toList <$> Data.Vector.replicateM n (i :: IO Int)

mutationSwaps :: Int -> IO [(Int, Int)]
mutationSwaps n = do
  p1 <- randomIndexes n (n - 1)
  p2 <- randomIndexes n (n - 1)
  return $ zip p1 p2

individuals :: [VIndividual]
individuals =
  map
    fromList
    [ [0, 1, 2, 3, 4, 5, 6, 9, 8, 7],
      [0 .. 9],
      [9,8..0],
      [9, 1, 2, 3, 4, 5, 6, 7, 8, 0]
    ]

test :: IO ()
test = do
  let points = fromList $ zip [0 .. 9] [0 .. 9]
  let sorted = sortGeneration $ evalFitness points individuals
  print sorted
  let selected = select (4, 0, 0) sorted
  print "Selected AF"
  print selected
  ci <- randomIndexes 2 9
  print "Heres the indexes"
  print ci
  let crossed = crossSelected ci selected
  print "crossed AF"
  print crossed

