import Data.Maybe ( mapMaybe )
import Data.List ( foldl1' )
import Text.Read ( readMaybe )

-- Split a string by comma delimiter
splitComma :: String -> [String]
splitComma s = case dropWhile (==',') s of
  "" -> []
  s' -> w : splitComma s''
        where (w, s'') = break (==',') s'

{------- PART 1 -------}
-- Parse input for part 1
parse1 :: String -> (Int, [Int])
parse1 s = (read t :: Int, ids)
  where
    (t : ids' : []) = lines s
    ids             = mapMaybe readMaybe (splitComma ids') :: [Int]

-- Get the ID of the earliest bus multiplied by the num of mins needed to wait
earliestBus :: (Int, [Int]) -> Int
earliestBus (t, ids)
  = (t' - t) * id
  where
    ts       = zip (map (\x -> (t `div` x + 1) * x) ids) ids
    (t', id) = minimum ts

{------- PART 2 -------}
-- Parse input for part 2
parse2 :: String -> [Integer]
parse2 s = ids
  where
    (_ : ids'' : []) = lines s
    ids'             = map readMaybe (splitComma ids'') :: [Maybe Integer]
    ids              = map (\n -> case n of
                              Just x  -> x
                              Nothing -> 0) ids'

-- Get equations for (a, n) in the form of 't = -a mod n'
getEqs :: [Integer] -> [(Integer, Integer)]
getEqs ns = getEqs' ns 0
  where
    getEqs' :: [Integer] -> Integer -> [(Integer, Integer)]
    getEqs' [] _ = []
    getEqs' (n : ns) cnt = case n of
      0 -> getEqs' ns (cnt + 1)
      x -> (cnt, x) : getEqs' ns (cnt + 1)

-- Calculate Bezout coefficients
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b
  | b == 0         = (1, 0)
  | otherwise      = (v', (u' - q * v'))
  where
    (q, r)   = quotRem a b
    (u', v') = bezout b r

-- Solve a pair of mod equations with the Chinese Remainder Theorem
solveEqPair :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solveEqPair (a1, n1) (a2, n2)
  = (a12, n1 * n2)
  where
    (m1, m2) = bezout n1 n2
    a12      = a1 * m2 * n2 + a2 * m1 * n1

-- Find the earliest timestamp that satisfies all mod equations
earliestTime :: [(Integer, Integer)] -> Integer
earliestTime ns = t
  where
    (a, n) = foldl1' solveEqPair ns
    t      = (abs a) `mod` n

main :: IO()
main
  = do
      s <- readFile "13.txt"

      putStrLn $ "Part 1: " ++ show ((earliestBus . parse1) s)
      putStrLn $ "Part 2: " ++ show ((earliestTime . getEqs . parse2) s)
