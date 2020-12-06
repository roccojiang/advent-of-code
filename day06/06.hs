import Data.Map (fromListWith, toList)

-- Combine input split by lines into their own separated customs items
combine :: [String] -> [[String]]
combine [] = []
combine (x : xs)
  = combine' x xs : combine (drop 1 $ dropWhile (""/=) xs)
  where
    combine' :: String -> [String] -> [String]
    combine' x xs = x : takeWhile (""/=) xs

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

distinct :: String -> String
distinct [] = []
distinct (c : cs)
 | not (c `elem` cs) = c : distinct cs
 | otherwise = distinct cs

main :: IO()
main
  = do
      s <- readFile "06.txt"
      let distincts = map distinct $ (map concat) (combine (lines s))

          strings = (combine (lines s))
          -- strings' = map (length . answers . frequency) strings

          strings'' = [["abc"], ["a", "b", "c"], ["ab", "ac"], ["a", "a", "a", "a"], ["b"]]
          ans = [s | s <- strings'']

          -- strings''' = map frequency' strings''
          -- strings''' = frequency $ (map.map) distinct strings''
          strings''' = frequency strings''
          -- strings'''' = map answers' strings'''

      print $ strings
      -- print $ strings''''
      
      putStrLn $ "Part 1: " ++ show (sum (map length distincts))
      -- putStrLn $ "Part 2: " ++ show (sum (map frequency' strings))