import Data.List ( sort, group )

-- PART 1
-- Calculates the difference between each consecutive pair of ratings.
getDiffs :: [Int] -> [Int]
getDiffs ns
  = zipWith subtract ns' (tail ns')
  where
      ns' = sort (0 : (maximum ns + 3) : ns)

-- Calculates the num of 1-jolt diffs multiplied by the num of 3-jolt diffs.
multDiff :: [Int] -> Int
multDiff ns
  = ones * threes
  where
      diffs  = getDiffs ns
      ones   = (length . filter (1==)) diffs
      threes = (length . filter (3==)) diffs

-- PART 2
-- Tribonacci numbers, starting with 1 1 2.
tribs :: [Integer]
tribs = 1 : 1 : 2 : zipWith (+) (zipWith (+) tribs (tail tribs)) (drop 2 tribs)

-- Calculate the number of distinct ways to arrange the adapters.
-- Explanation:
-- All adapters are spaced either 1 or 3 jolts apart.
-- The number of arrangements for a chain of length n of consecutive 1 jolt
-- differences corresponds to the nth term in the tribonacci sequence.
-- Thus we multiply these terms together to find the total arrangements.
countArrs :: [Int] -> Integer
countArrs = product
          . map (\x -> tribs !! length x)
          . filter (1 `elem`)
          . group
          . getDiffs

main :: IO()
main
  = do
      s <- readFile "10.txt"
      let ns = map read (lines s) :: [Int]

      putStrLn $ "Part 1: " ++ show (multDiff ns)
      putStrLn $ "Part 2: " ++ show (countArrs ns)
