-- Checks for three unique entries that sum to 2020.
-- A solution is guaranteed to exist.
-- Returns the product of those three entries.
findEntries :: [Int] -> Int
findEntries (x : xs) = case findEntries' (2020 - x) xs of
  Just y  -> x * y
  Nothing -> findEntries xs

-- Checks for two unique entries that sum to n.
-- If they exist, return Just the product of the two entries; otherwise Nothing.
findEntries' :: Int -> [Int] -> Maybe Int
findEntries' _ [] = Nothing
findEntries' n (x : xs)
  | elem diff xs = Just (x * diff)
  | otherwise    = findEntries' n xs
  where
    diff = n - x

main :: IO()
main
  = do
      s <- readFile "01.txt"
      let ns   = map (read :: String -> Int) (lines s)
          prod = findEntries ns
      putStrLn $ show prod
