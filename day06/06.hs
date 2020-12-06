import Data.List (intersect, nub)

-- Combine input split by double \n into their own separated lists
combine :: [String] -> [[String]]
combine [] = []
combine (x : xs)
  = combine' x xs : combine (drop 1 $ dropWhile (""/=) xs)
  where
    combine' :: String -> [String] -> [String]
    combine' x xs = x : takeWhile (""/=) xs

-- Sum the count of questions to which *anyone* answered yes
count1 :: [[String]] -> Int
count1 = sum . map (length . nub . concat)

-- Sum the count of questions to which *everyone* answered yes
count2 :: [[String]] -> Int
count2 = sum . map (length . foldr1 intersect)

main :: IO()
main
  = do
      s <- readFile "06.txt"
      let groups = combine (lines s)
      
      putStrLn $ "Part 1: " ++ show (count1 groups)
      putStrLn $ "Part 2: " ++ show (count2 groups)
