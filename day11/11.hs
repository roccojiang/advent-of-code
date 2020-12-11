import Data.Map.Strict ( Map, (!?) )
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

-- Parse input into a map with coordinates for every seat
parse :: [String] -> Map Coord Char
parse s
  = Map.fromList ss
  where
    ss = [((y, x), c) | (y, row) <- zip [0..] s, (x, c) <- zip [0..] row]

-- Apply the seat-changing rules of part 1, once
applyRound1 :: Map Coord Char -> Map Coord Char
applyRound1 ss = Map.mapWithKey updateSeat ss
  where
    updateSeat :: Coord -> Char -> Char
    updateSeat (r, c) ch
      | ch == 'L' && adj == 0 = '#'
      | ch == '#' && adj >= 4 = 'L'
      | otherwise             = ch
      where
        adj = (length . filter findAdj)
              [ (r, c-1)
              , (r+1, c-1)
              , (r+1, c)
              , (r+1, c+1)
              , (r, c+1)
              , (r-1, c+1)
              , (r-1, c)
              , (r-1, c-1)
              ]

        findAdj :: Coord -> Bool
        findAdj coord = case (ss !? coord) of
          Just '#' -> True
          _        -> False

-- Apply the seat-changing rules of part 2, once
applyRound2 :: Map Coord Char -> Map Coord Char
applyRound2 ss = Map.mapWithKey updateSeat ss
  where
    updateSeat :: Coord -> Char -> Char
    updateSeat coord ch
      | ch == 'L' && adj == 0 = '#'
      | ch == '#' && adj >= 5 = 'L'
      | otherwise             = ch
      where
      adj = (length . filter (look coord))
            [ (0, -1)
            , (1, -1)
            , (1, 0)
            , (1, 1)
            , (0, 1)
            , (-1, 1)
            , (-1, 0)
            , (-1, -1)
            ]

      look :: Coord -> Coord -> Bool
      look (r, c) grad@(dr, dc) = case (ss !? (r + dr, c + dc)) of
        Just '.' -> look (r + dr, c + dc) grad
        Just '#' -> True
        _   -> False

-- Count the number of occupied seats after equilibrium is reached
count :: (Map Coord Char -> Map Coord Char) -> String -> Int
count f s = (Map.size . Map.filter (=='#')) ss'
  where
    s'   = lines s

    ss   = parse s'
    ss'  = until (\x -> f x == x) f ss

main :: IO()
main
  = do
      s <- readFile "11.txt"
      putStrLn $ "Part 1: " ++ show (count applyRound1 s)
      putStrLn $ "Part 2: " ++ show (count applyRound2 s)
