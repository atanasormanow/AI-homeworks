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

-- -- IO
genPoints :: Int -> IO (Vector Point)
genPoints n =
  let genPoint = getStdRandom $ randomR ((-n, -n), (n, n))
   in Data.Vector.replicateM n (genPoint :: IO Point)

permuteFromTo :: Int -> Int -> IO VIndividual
permuteFromTo m n = (shuffleM . fromList) [m .. n]

population :: Int -> Int -> IO [VIndividual]
population n = flip Control.Monad.replicateM $ permuteFromTo 0 (n - 1)

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

evalFitness :: Vector Point -> [VIndividual] -> [ScoredVIndividual]
evalFitness ps = Prelude.map (\v -> (v, score ps v))

sortGeneration :: [ScoredVIndividual] -> [ScoredVIndividual]
sortGeneration inds =
  let totalScore = Prelude.foldl (\acc (_, s) -> acc + s) 0 inds
      cmp (_, f1) (_, f2) = compare (f1 / totalScore) (f2 / totalScore)
   in sortBy cmp inds

select :: [ScoredVIndividual] -> ([VIndividual], [VIndividual])
select xs =
  let sorted = sortGeneration xs
      (top70, bot30) = Prelude.splitAt 70 sorted
      new = Prelude.map fst top70
      old = Prelude.map fst $ take 20 top70 Prelude.++ drop 20 bot30
   in (new, old)

crossOp :: Int -> VIndividual -> VIndividual -> [VIndividual]
crossOp i parent1 parent2 =
  let (pref1, _) = Data.Vector.splitAt i parent1
      (pref2, _) = Data.Vector.splitAt i parent2
      child1 = pref1 Data.Vector.++ vdiff parent2 pref1
      child2 = pref2 Data.Vector.++ vdiff parent1 pref2
   in [child1, child2]

-- TODO
cross :: [Int] -> [VIndividual] -> [VIndividual] -> [VIndividual]
cross ps1 ps2 = Prelude.concat . Prelude.zipWith3 crossOp ps1 ps2

crossSelected :: ([VIndividual], [VIndividual]) -> [VIndividual]
crossSelected (selected, survivors) =
  let (p1, p2) = uninterleave selected
   in survivors Prelude.++ cross (repeat 10) p1 p2

swap :: (Int, Int) -> VIndividual -> VIndividual
swap (i, j) ind = ind // [(i, ind ! j), (j, ind ! i)]

mutate :: [(Int, Int)] -> [VIndividual] -> [VIndividual]
mutate = Prelude.zipWith swap

evolve :: Vector Point -> [(Int, Int)] -> [VIndividual] -> [VIndividual]
evolve points mutations =
  mutate mutations
    . crossSelected
    . select
    . sortGeneration
    . evalFitness points

mutationSwaps :: Int -> IO [(Int, Int)]
mutationSwaps n = do
  p1 <- permuteFromTo 0 (n - 1)
  p2 <- permuteFromTo 0 (n - 1)
  return $ Prelude.zip (toList p1) (toList p2)

main :: IO ()
main = do
  putStrLn "Enter the number of points the salesman should visit:"
  line <- getLine
  let n = (read line::Int)
  let populationSize = 100 :: Int
  points <- genPoints n
  individuals <- population n populationSize
  mutations <- mutationSwaps n
  print $ evolve points mutations individuals

-- TODO:
-- - move some functions in a separate module
-- - since paths containing all nodes are permutations of those nodes
--    then it useless to have a generation with more than n! individuals,
--    where n is the number of nodes (genes of individuals)
-- - selection proportions: cross 70to -> 70new; 30new = 20to + 10bo
--    (ill assume i have 100 individuals for now)
-- - minimize conversions between [a] and (Vector a)
