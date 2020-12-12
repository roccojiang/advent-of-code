type Instr = (Char, Int)
type Coord = (Int, Int)
type Angle = Int

parse :: [String] -> [Instr]
parse = map (\s -> (head s, read (tail s) :: Int))

follow :: [Instr] -> Coord
follow is = follow' (0, 0) is 0
  where 
    follow' :: Coord -> [Instr] -> Angle -> Coord
    follow' coord [] _ = coord
    follow' coord@(x, y) ((act, val) : is) ang = case act of
      'N' -> follow' (x, y + val) is ang
      'S' -> follow' (x, y - val) is ang
      'E' -> follow' (x + val, y) is ang
      'W' -> follow' (x - val, y) is ang
      'L' -> follow' coord is ((ang - val) `mod` 360)
      'R' -> follow' coord is ((ang + val) `mod` 360)
      'F' -> case ang of
        0   -> follow' (x + val, y) is ang
        90  -> follow' (x, y - val) is ang
        180 -> follow' (x - val, y) is ang
        270 -> follow' (x, y + val) is ang

getDistance :: Coord -> Int
getDistance (x, y) = abs x + abs y

main :: IO()
main
  = do
      s <- readFile "12.txt"
      let ls = lines s

          t = ["F10","N3","F7","R90","F11"]

      print $ (getDistance . follow . parse) ls
