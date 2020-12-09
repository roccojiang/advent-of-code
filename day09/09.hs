-- Find the invalid num that is not a sum of 2 nums in preamble (with length l).
findNum :: [Int] -> Int -> Int
findNum ns l = findNum' (take l ns) (drop l ns) ns
  where
    findNum' :: [Int] -> [Int] -> [Int] -> Int
    findNum' preamb (n : ns) ns'
      | isSum     = findNum' nextPreamb ns (drop 1 ns')
      | otherwise = n
      where
        diffs      = map (n-) preamb
        isSum      = (or . map (`elem` preamb)) diffs
        nextPreamb = take l (drop 1 ns')

-- Find the encryption weakness by adding up the smallest and largest
-- contiguous numbers that sum to the invalid number given in findNum.
-- Assumes this contiguous range does exist.
findWeak :: [Int] -> Int -> Int
findWeak ns num
  | num `elem` prefixSum = minimum lSum + maximum lSum
  | otherwise            = findWeak (drop 1 ns) num
  where
    prefixSum = takeWhile (<=num) $ scanl1 (+) ns
    lSum      = take (length prefixSum) ns

main :: IO()
main
  = do
      s <- readFile "09.txt"
      let ns = map read (lines s) :: [Int]
          n  = findNum ns 25

      putStrLn $ "Part 1: " ++ show n
      putStrLn $ "Part 2: " ++ show (findWeak ns n)
