import Text.Read ( readMaybe )

-- Split a string by comma delimiter
splitComma :: String -> [String]
splitComma s = case dropWhile (==',') s of
  "" -> []
  s' -> w : splitComma s''
        where (w, s'') = break (==',') s'

-- Parse input
parse :: String -> (Int, [Int])
parse s
  = (read t :: Int, ids)
  where
    (t : ids'' : []) = lines s
    ids'             = map readMaybe (splitComma ids'') :: [Maybe Int]
    ids              = [x | Just x <- ids']

-- Get the ID of the earliest bus multiplied by the num of mins needed to wait
earliestBus :: (Int, [Int]) -> Int
earliestBus (t, ids)
  = (t' - t) * id
  where
    ts       = zip (map (\x -> (t `div` x + 1) * x) ids) ids
    (t', id) = minimum ts

main :: IO()
main
  = do
      s <- readFile "13.txt"
      let s' = parse s
          t  = (939, [7,13,59,31,19])

      putStrLn $ "Part 1: " ++ show (earliestBus s')
