module Main (main) where

import Lib

import Data.Vector (Vector, replicateM, fromList, toList)
import Immutable.Shuffle (shuffleM)
import System.Random (getStdRandom, randomR)

type Individual = Vector Int

type Fitness = Float

type ScoredIndividuals = (Individual, Fitness)

type Point = (Int, Int)

-- type Selection = Float

data Pair = Pair (Individual, Individual) Int

generatePoints :: Int -> IO (Vector Point)
generatePoints n =
  let genPoint = getStdRandom $ randomR ((-n, -n), (n, n))
   in replicateM n (genPoint :: IO Point)

newIndividual :: Int -> IO Individual
newIndividual n = (shuffleM . fromList) [1 .. n]

newGeneration :: Int -> Int -> IO (Vector Individual)
newGeneration genSize n = replicateM genSize (newIndividual n)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in (sqrt . fromIntegral) (dx * dx + dy * dy)

-- TODO: try to do this avoiding conversion
score :: Individual -> Fitness
score = score' . toList

score' :: [Int] -> Fitness
score'  = undefined

-- Generation steps:
evalFitness :: [Individual] -> [ScoredIndividuals]
evalFitness = map (\v -> (v, score v))

select :: [ScoredIndividuals] -> [Individual]
select = undefined

-- does this depend on selection?
pair :: [Individual] -> [Pair]
pair = undefined

cross :: [Pair] -> [Individual]
cross = undefined

mutate :: [Individual] -> [Individual]
mutate = undefined

main :: IO ()
main = do
  points <- generatePoints 10
  print points

-- TODO:
-- - take user input
-- - move some functions in a separate module
-- - since paths containing all nodes are permutations of those nodes
--    then it useless to have a generation with more than n! individuals,
--    where n is the number of nodes (number of genes an individual has)
