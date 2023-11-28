treeTraverse :: [String] -> Int -> Int -> Int -> Int
treeTraverse ls right down len
  = length [l | (l, i) <- zip (drop down ls) [0..],
                          let i' = i `div` down,
                          l !! ((right * (i' + 1)) `mod` len) == '#',
                          i `mod` down == 0]

main :: IO()
main
  = do
      s <- readFile "03.txt"
      let ls = lines s
          len = length $ head ls
          slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

      putStrLn $ "Part 1: " ++ (show $ treeTraverse ls 3 1 len)
      putStrLn $ "Part 2: " ++ (show $
                                product $
                                map (\(x, y) -> treeTraverse ls x y len) slopes)
