type Coord = (Int, Int)
type Seat  = (Coord, Char)

applyRound :: [Seat] -> [Seat]
applyRound ss
  = map updateSeat ss
  where
    rLen = 96
    rNum = 99
    -- rLen = 10
    -- rNum = 10

    updateSeat :: Seat -> Seat
    updateSeat s@(coord@(x, y), c)
      | c == 'L' && adjs == 0 = (coord, '#')
      | c == '#' && adjs >= 4 = (coord, 'L')
      | otherwise             = s
      where
        sN   = checkSeat x (y - 1)
        sNE  = checkSeat (x + 1) (y - 1)
        sE   = checkSeat (x + 1) y
        sSE  = checkSeat (x + 1) (y + 1)
        sS   = checkSeat x (y + 1)
        sSW  = checkSeat (x - 1) (y + 1)
        sW   = checkSeat (x - 1) y
        sNW  = checkSeat (x - 1) (y - 1)

        adjs = sum [sN, sNE, sE, sSE, sS, sSW, sW, sNW]

        checkSeat :: Int -> Int -> Int
        checkSeat x y
          | x < 0 || y < 0 || x >= rLen || y >= rNum = 0
          | snd (ss !! (rLen * y + x)) == '#' = 1
          | otherwise = 0

main :: IO()
main
  = do
      s <- readFile "11.txt"
      let s' = lines s
          ss = [((x, y), c) | (y, row) <- zip [0..] s', (x, c) <- zip [0..] row]
          ss' = until (\x -> applyRound x == x) applyRound ss

          t  = ["L.LL.LL.LL","LLLLLLL.LL","L.L.L..L..","LLLL.LL.LL","L.LL.LL.LL","L.LLLLL.LL","..L.L.....","LLLLLLLLLL","L.LLLLLL.L","L.LLLLL.LL"]
          t' = [((x, y), c) | (y, row) <- zip [0..] t, (x, c) <- zip [0..] row]
      
      -- Warning: this takes ~4 minutes!
      putStrLn $ "Part 1: " ++ show (length [s | s <- ss', snd s == '#'])
