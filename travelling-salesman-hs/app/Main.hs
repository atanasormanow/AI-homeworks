module Main (main) where

import Data.List (sortBy, (\\))
import Data.Vector hiding ((++))
import Immutable.Shuffle (shuffleM)
import Lib
import System.Random (getStdRandom, randomR)
import Prelude hiding (head, tail)
import Control.Monad (replicateM)

type VIndividual = Vector Int

type Fitness = Float

type ScoredVIndividual = (VIndividual, Fitness)

type Point = (Int, Int)

type Pair = (VIndividual, VIndividual)

data Selection = Selection [VIndividual] [ScoredVIndividual]

-- IO
generatePoints :: Int -> IO (Vector Point)
generatePoints n =
  let genPoint = getStdRandom $ randomR ((-n, -n), (n, n))
   in Data.Vector.replicateM n (genPoint :: IO Point)

newVIndividual :: Int -> IO VIndividual
newVIndividual n = (shuffleM . fromList) [0 .. n-1]

newGeneration :: Int -> Int -> IO [VIndividual]
newGeneration n = flip Control.Monad.replicateM (newVIndividual n)
-----

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in (sqrt . fromIntegral) (dx * dx + dy * dy)

-- TODO: test
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

-- TODO: think of a better 10
select :: [ScoredVIndividual] -> Selection
select xs =
  let totalScore = Prelude.foldl (\acc (_, s) -> acc + s) 0 xs
      (top70, bot30) =
        (Prelude.splitAt 70 . sortBy (flip (\(_, f1) (_, f2) -> compare f1 f2))) xs
      new = Prelude.map fst top70
      -- concat with the shorter list on the left
      old = Prelude.drop 20 bot30 ++ Prelude.take 20 top70
   in Selection new old


crossOp :: Int -> Pair -> Pair
crossOp i (parent1,parent2) =
  let (pref1, suff1) = Data.Vector.splitAt i parent1
      (pref2, suff2) = Data.Vector.splitAt i parent2
      child1 = pref1 +v+ vdiff parent2 pref1
      child2 = pref2 +v+ vdiff parent1 pref2
   in (fromList [], fromList [])

-- TODO
cross :: [VIndividual] -> [VIndividual] -> [Pair]
cross = Prelude.zipWith (crossOp 5)

mutate :: [VIndividual] -> [VIndividual]
mutate = undefined

main :: IO ()
main = do
  points <- generatePoints 5
  print points
  individual <- newVIndividual 5
  print individual
  print $ score points individual

-- TODO:
-- - take user input
-- - move some functions in a separate module
-- - since paths containing all nodes are permutations of those nodes
--    then it useless to have a generation with more than n! individuals,
--    where n is the number of nodes (number of genes an individual has)
-- - selection proportions: cross 70to -> 70new; 30new = 20to + 10bo
--    (ill assume i have 100 individuals for now)
