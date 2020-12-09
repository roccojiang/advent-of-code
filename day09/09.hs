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
        isSum = or $ map (`elem` preamb) diffs
        nextPreamb = take l (drop 1 ns')

main :: IO()
main
  = do
      s <- readFile "09.txt"
      let s' = map read (lines s) :: [Int]
          t = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

      putStrLn $ "Part 1: " ++ show (findNum s' 25)
