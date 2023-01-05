module Main (main) where

import Data.Vector (Vector, fromList, map)
import Lib

citiesXY :: Vector (Float, Float)
citiesXY =
  fromList
    [ (0.190032E-03, -0.285946E-03),
      (383.458, -0.608756E-03),
      (-27.0206, -282.758),
      (335.751, -269.577),
      (69.4331, -246.780),
      (168.521, 31.4012),
      (320.350, -160.900),
      (179.933, -318.031),
      (492.671, -131.563),
      (112.198, -110.561),
      (306.320, -108.090),
      (217.343, -447.089)
    ]

citiesName :: [String]
citiesName =
  [ "Aberystwyth",
    "Brighton",
    "Edinburgh",
    "Exeter",
    "Glasgow",
    "Inverness",
    "Liverpool",
    "London",
    "Newcastle",
    "Nottingham",
    "Oxford",
    "Stratford"
  ]

testBest :: IO ()
testBest = do
  print "Best individual below:"
  print $ evalFitness citiesXY [fromList [0, 5, 9, 4, 2, 7, 11, 3, 6, 10, 1, 8]]

testWithCities :: IO ()
testWithCities = do
  let n = 12
  let populationSize = 100
  let proportions = (70, 20, 10)
  let points = citiesXY
  mutations <- mutationSwaps populationSize n
  individuals <- population n populationSize
  print individuals
  -- let generations = iterate (evolve points proportions mutations) individuals
  -- let k = 100
  -- let (ind, f) = head . sortPopulation . evalFitness points $ generations !! k
  -- print $ Data.Vector.map (citiesName !!) ind

main :: IO ()
main = do
  putStrLn "Enter the number of points the salesman should visit:"
  line <- getLine
  let n = (read line :: Int)
  -- Constants
  let populationSize = 100 :: Int
  let proportions = (70, 20, 10)
  -- Points are with x,y in [-n, n]
  points <- genPoints n (fromIntegral n)
  mutations <- mutationSwaps populationSize n
  individuals <- population n populationSize
  let best = head . sortPopulation . evalFitness points
  let gen1 = best individuals
  print ("Best path is " ++ show (snd gen1) ++ " long in generation 1")
  let generations = iterate (evolve points proportions mutations) individuals
  let lastGen = 100
  let (_, f) =
        head . sortPopulation . evalFitness points $ generations !! lastGen
  print ("Best path is " ++ show f ++ " long in generation " ++ show lastGen)

-- TODO:
-- - minimize conversions between [a] and (Vector a)
-- - ? include weak individuals in the breeding selection
-- - ? mutate by swapping i-1 and i+1 for some i instead of random indexes
