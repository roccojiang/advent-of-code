treeTraverseDown1 :: [String] -> Int -> Int -> Int
treeTraverseDown1 ls right len
  = length [l | (l, i) <- zip (drop 1 ls) [0..], l !! ((right * (i + 1)) `mod` len) == '#']

treeTraverseDown2 :: [String] -> Int -> Int -> Int
treeTraverseDown2 ls right len
  = length [l | (l, i) <- zip (drop 2 ls) [0..], l !! ((right * ((i `div` 2) + 1)) `mod` len) == '#', even i]

main :: IO()
main
  = do
      s <- readFile "03.txt"
      let ls = lines s
          len = length $ head ls
          r1d1 = treeTraverseDown1 ls 1 len
          r3d1 = treeTraverseDown1 ls 3 len
          r5d1 = treeTraverseDown1 ls 5 len
          r7d1 = treeTraverseDown1 ls 7 len
          r1d2 = treeTraverseDown2 ls 1 len

          testls = ["..##.......", "#...#...#..", ".#....#..#.", "..#.#...#.#", ".#...##..#.", "..#.##.....", ".#.#.#....#", ".#........#", "#.##...#...", "#...##....#", ".#..#...#.#"]
          testlen = length $ head testls
      putStrLn $ show $ product [r1d1, r3d1, r5d1, r7d1, r1d2]
