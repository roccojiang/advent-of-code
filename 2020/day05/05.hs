type Seat = (Int, Int)

toSeat :: String -> Seat
toSeat s
  = (row, col)
  where
    (sRow, sCol) = splitAt 7 s
    bsRow        = map (\c -> if c == 'F' then '0' else '1') sRow
    bsCol        = map (\c -> if c == 'L' then '0' else '1') sCol
    (row, col)   = (binToDec (read bsRow :: Int), binToDec (read bsCol :: Int))

    binToDec :: Int -> Int
    binToDec 0 = 0
    binToDec n = 2 * binToDec(n `div` 10) + n `mod` 10

seatID :: Seat -> Int
seatID (row, col) = row * 8 + col

missingID :: [Int] -> Int
missingID ns
  = idSum' - idSum
  where
    idSum  = sum ns
    min    = minimum ns
    max    = maximum ns
    idSum' = ((max * (max + 1)) - ((min - 1) * min)) `div` 2

main :: IO()
main
  = do
      s <- readFile "05.txt"
      let ids = map (seatID . toSeat) (lines s)
      putStrLn $ "Part 1: " ++ show (maximum ids)
      putStrLn $ "Part 2: " ++ show (missingID ids)
