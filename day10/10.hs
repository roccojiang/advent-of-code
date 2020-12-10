import Data.List ( sort )

countDiff :: [Int] -> Int
countDiff ns
  = ones * threes
  where
      ns'    = sort ns
      ps     = zip ns' (tail ns')
      diffs  = map (uncurry subtract) ps
      ones   = (length . filter (1==)) diffs + 1
      threes = (length . filter (3==)) diffs + 1

main :: IO()
main
  = do
      s <- readFile "10.txt"
      let ns = map read (lines s) :: [Int]
      
      putStrLn $ "Part 1: " ++ show (countDiff ns)
