-- Find the first number that is not a sum of 2 nums in preamble (with length l)
findNum :: [Int] -> Int -> Int
findNum ns l = findNum' (take l ns) (drop l ns) ns
  where
    findNum' :: [Int] -> [Int] -> [Int] -> Int
    findNum' preamb (n : ns) ns'
      | isSum     = findNum' nextPreamb ns (drop 1 ns')
      | otherwise = n
      where
        diffs = map (n-) preamb
        isSum = (or . map (`elem` preamb)) diffs
        nextPreamb = take l (drop 1 ns')

-- Find the encryption weakness by adding up the smallest and largest
-- contiguous numbers that sum to the invalid number given in findNum
findWeak :: [Int] -> Int -> Int
findWeak ns num
  = findWeak' ns []
  where
    findWeak' :: [Int] -> [Int] -> Int
    findWeak' (n : ns) nsPrev
      | 0 `elem` subtrs = minimum lSum + maximum lSum
      | otherwise       = findWeak' ns (n : nsPrev)
      where
        subtrs = takeWhile (>=0) $ scanl (-) num nsPrev
        lSum   = take (length subtrs - 1) nsPrev

main :: IO()
main
  = do
      s <- readFile "09.txt"
      let ns = map read (lines s) :: [Int]
          invalidNum = findNum ns 25

      putStrLn $ "Part 1: " ++ show invalidNum
      putStrLn $ "Part 2: " ++ show (findWeak ns invalidNum)
