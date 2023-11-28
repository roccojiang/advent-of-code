type Instr = (Char, Int)
type Coord = (Int, Int)
type Angle = Int

-- Parse input lines into a list of instructions
parse :: [String] -> [Instr]
parse = map (\s -> (head s, read (tail s) :: Int))

-- Follows instructions based on part 1
follow1 :: [Instr] -> Coord
follow1 is = follow (0, 0) is 0
  where 
    follow :: Coord -> [Instr] -> Angle -> Coord
    follow coord [] _ = coord
    follow coord@(x, y) ((act, val) : is) ang = case act of
      'N' -> follow (x, y + val) is ang
      'S' -> follow (x, y - val) is ang
      'E' -> follow (x + val, y) is ang
      'W' -> follow (x - val, y) is ang
      'L' -> follow coord is ((ang - val) `mod` 360)
      'R' -> follow coord is ((ang + val) `mod` 360)
      'F' -> case ang of
        0   -> follow (x + val, y) is ang
        90  -> follow (x, y - val) is ang
        180 -> follow (x - val, y) is ang
        270 -> follow (x, y + val) is ang

-- Follows instructions based on part 2
follow2 :: [Instr] -> Coord
follow2 is = follow (0, 0) is (10, 1)
  where 
    follow :: Coord -> [Instr] -> Coord -> Coord
    follow coord [] _ = coord
    follow coord@(x, y) ((act, val) : is) waypoint@(e, n) = case act of
      'N' -> follow coord is (e, n + val)
      'S' -> follow coord is (e, n - val)
      'E' -> follow coord is (e + val, n)
      'W' -> follow coord is (e - val, n)
      'L' -> case val of
        90  -> follow coord is (-n, e)
        180 -> follow coord is (-e, -n)
        270 -> follow coord is (n, -e)
      'R' -> case val of
        90  -> follow coord is (n, -e)
        180 -> follow coord is (-e, -n)
        270 -> follow coord is (-n, e)
      'F' -> follow (x + val * e, y + val * n) is waypoint

-- Calculate the Manhattan distance from a coordinate
getDistance :: Coord -> Int
getDistance (x, y) = abs x + abs y

main :: IO()
main
  = do
      s <- readFile "12.txt"

      putStrLn $ "Part 1: " ++ show ((getDistance . follow1 . parse . lines) s)
      putStrLn $ "Part 2: " ++ show ((getDistance . follow2 . parse . lines) s)
