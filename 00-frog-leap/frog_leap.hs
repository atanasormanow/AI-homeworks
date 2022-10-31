import Data.List (find, nub)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

data SearchProblem a = SearchProblem
  { start :: a,
    expand :: a -> [a],
    isDone :: a -> Bool
  }

moveL1 :: String -> Maybe String
moveL1 [] = Nothing
moveL1 [x] = Nothing
moveL1 ('l' : '_' : xs) = Just $ '_' : 'l' : xs
moveL1 (x : xs) = (x :) <$> moveL1 xs

moveR1 :: String -> Maybe String
moveR1 [] = Nothing
moveR1 [x] = Nothing
moveR1 ('_' : 'r' : xs) = Just $ 'r' : '_' : xs
moveR1 (x : xs) = (x :) <$> moveR1 xs

moveL2 :: String -> Maybe String
moveL2 [] = Nothing
moveL2 [x] = Nothing
moveL2 [x, y] = Nothing
moveL2 ('l' : x : '_' : xs) = Just $ '_' : x : 'l' : xs
moveL2 (x : xs) = (x :) <$> moveL2 xs

moveR2 :: String -> Maybe String
moveR2 [] = Nothing
moveR2 [x] = Nothing
moveR2 [x, y] = Nothing
moveR2 ('_' : x : 'r' : xs) = Just $ 'r' : x : '_' : xs
moveR2 (x : xs) = (x :) <$> moveR2 xs

moves :: String -> [String]
moves board = mapMaybe ($ board) [moveL1, moveL2, moveR1, moveR2]

-- TODO: remember stone index to avoid linear traversal
frogLeap :: String -> SearchProblem String
frogLeap s =
  SearchProblem
    { start = s,
      expand = moves,
      isDone = (reverse s ==)
    }

dfs :: (Eq a) => SearchProblem a -> Maybe [a]
dfs (SearchProblem start expand isDone) = loop start
  where
    loop node
      | isDone node = Just [node]
      | otherwise = (node :) <$> listToMaybe (mapMaybe loop $ expand node)

main = do
  putStrLn "Enter the frog leap puzzle start state: "
  puzzle <- getLine
  putStrLn "The steps to the solution are as follows:"
  putStr $ unlines $ fromJust $ dfs $ frogLeap puzzle
