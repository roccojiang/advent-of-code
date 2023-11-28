-- Checks for the two unique entries that sum to 2020.
-- A solution is guaranteed to exist.
-- Returns the product of those two entries.
findEntries :: [Int] -> Int
findEntries (x : xs)
  | elem diff xs = x * diff
  | otherwise    = findEntries xs
  where
    diff = 2020 - x

-- Alternate solution using list comprehensions.
findEntries' :: [Int] -> Int
findEntries' xs = head [x * y | x <- xs, let y = 2020 - x, y `elem` xs]

main :: IO()
main
  = do
      s <- readFile "01.txt"
      let ns   = map (read :: String -> Int) (lines s)
          prod = findEntries' ns
      putStrLn $ show prod
