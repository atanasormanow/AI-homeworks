module Main (main) where

import Lib (genPoints, population, mutationSwaps, evolve)
import Data.Vector (fromList)

main :: IO ()
main = do
  putStrLn "Enter the number of points the salesman should visit:"
  -- line <- getLine
  -- let n = (read line::Int)
  -- let populationSize = 100 :: Int
  -- let points = fromList $ zip [0..10] [0..10]
  -- individuals <- population n populationSize
  -- print "Here are the initial individuals:"
  -- print individuals
  -- mutations <- mutationSwaps n
  -- print "Here is the second generation:"
  -- print $ evolve points (70, 20, 10) mutations individuals

  -- points <- genPoints n

-- TODO:
-- - since paths containing all nodes are permutations of those nodes
--    then it useless to have a generation with more than n! individuals,
--    where n is the number of nodes (genes of individuals)
-- - selection proportions: cross 70to -> 70new; 30new = 20to + 10bo
--    (ill assume i have 100 individuals for now)
-- - minimize conversions between [a] and (Vector a)
