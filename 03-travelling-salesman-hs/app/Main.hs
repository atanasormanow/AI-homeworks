module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn "Enter the number of points the salesman should visit:"
  line <- getLine
  let n = (read line::Int)
  let populationSize = 100 :: Int
  let proportions = (70,20,10)
  points <- genPoints n
  mutations <- mutationSwaps n
  individuals <- population n populationSize
  let nextGeneration = evolve points proportions mutations individuals
  print $ sortPopulation $ evalFitness points nextGeneration


-- TODO:
-- - minimize conversions between [a] and (Vector a)
