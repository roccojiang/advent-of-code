treeTraverse :: [String] -> Int -> Int
treeTraverse ls len
  = length [l | (l, i) <- zip (drop 1 ls) [0..], l !! ((3 * (i + 1)) `mod` len) == '#']

main :: IO()
main
  = do
      s <- readFile "03.txt"
      let ls = lines s
      print $ treeTraverse ls (length $ head ls)
