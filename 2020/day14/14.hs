import Data.Char ( isDigit, digitToInt )
import Data.List ( foldl' )
import Data.Bits ( (.&.), (.|.) )
import Data.Map ( Map )
import qualified Data.Map as Map

type Address = Int
type Value   = Int
type Mask    = (Int, Int)

createMask :: String -> Mask
createMask s
  = (ones, zeros)
  where
    ones  = toDec $ map (\c -> if c == 'X' then '1' else c) s
    zeros = toDec $ map (\c -> if c == 'X' then '0' else c) s

    toDec :: String -> Int
    toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parse :: [String] -> [(String, [(Address, Value)])]
parse [] = []
parse (s : ss)
  = (mask, writes) : parse s''
  where
    mask = drop 7 s
    s'  = takeWhile (\s -> take 4 s /= "mask") ss
    writes = map (\s -> let (x, y) = break (=='=') s
                        in ( read $ filter isDigit x
                           , read $ filter isDigit y )) s'
    s'' = dropWhile (\s -> take 4 s /= "mask") ss

writeMem :: [(Mask, [(Address, Value)])] -> Map Address Value
writeMem [] = Map.empty
writeMem (((ones, zeros), avs) : is)
  = Map.union (writeMem is) (Map.fromList avs')
  where
    avs' = map (\(add, val) -> (add, val .&. ones .|. zeros)) avs

sumMem :: [(String, [(Address, Value)])] -> Int
sumMem xs
  = Map.foldl' (+) 0 (writeMem xs')
  where
    xs' = map (\(s, ys) -> (createMask s, ys)) xs

main :: IO()
main
  = do
      s <- readFile "14.txt"
      let s' = lines s
          t = ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X","mem[8] = 11","mem[7] = 101","mem[8] = 0"]
          t' = parse t
          s'' = map (\(s, ys) -> (createMask s, ys)) (parse s')

      putStrLn $ "Part 1: " ++ show ((sumMem . parse) s')
