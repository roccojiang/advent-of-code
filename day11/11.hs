import Data.Map ( Map, (!) )
import qualified Data.Map as Map

type Coord = (Int, Int)
type Seat  = (Coord, Char)

parse :: [String] -> Map Coord Char
parse s
  = Map.fromList ss
  where
    ss = [((y, x), c) | (y, row) <- zip [0..] s, (x, c) <- zip [0..] row]

count1 :: String -> Int
count1 s = (Map.size . Map.filter (=='#')) ss'
  where
    s'   = lines s
    rNum = length s'
    cNum = length (head s')

    ss   = parse s'
    ss'  = until (\x -> applyRound x == x) applyRound ss

    applyRound :: Map Coord Char -> Map Coord Char
    applyRound ss = Map.mapWithKey updateSeat ss
      where
        updateSeat :: Coord -> Char -> Char
        updateSeat (r, c) ch
          | ch == 'L' && adj == 0 = '#'
          | ch == '#' && adj >= 4 = 'L'
          | otherwise             = ch
          where
            adj = (length . filter (\(r, c) -> 
                    r >= 0 && r < rNum && c >= 0 && c < cNum
                    && ss ! (r, c) == '#'))
                  [ (r, c-1)
                  , (r+1, c-1)
                  , (r+1, c)
                  , (r+1, c+1)
                  , (r, c+1)
                  , (r-1, c+1)
                  , (r-1, c)
                  , (r-1, c-1)
                  ]

main :: IO()
main
  = do
      s <- readFile "11.txt"
      
      putStrLn $ "Part 1: " ++ show (count1 s)
