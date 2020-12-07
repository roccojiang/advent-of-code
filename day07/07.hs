import Text.Read (readMaybe)
import Data.List (nub)
import Data.Map (fromList, (!))

type Colour   = String
type Contents = [(Colour, Int)]
type Rule     = (Colour, Contents)

-- Parse one line of the input into a Rule
getRule :: String -> Rule
getRule s
  = (bag, contents)
  where
    ws = words s
    bag = (unwords . take 2) ws
    contents = getContents (drop 4 ws)
    
    getContents :: [String] -> Contents
    getContents [] = []
    getContents (s : ss)
      | s == "no" = []
      | otherwise = case (readMaybe s :: Maybe Int) of
        Just x  -> (clr, x) : getContents next
        Nothing -> []
        where
          clr = (unwords . take 2) ss
          next = drop 3 ss

-- Count the bag colours that contain the specified bag colour
coloursContaining :: Colour -> [Rule] -> [Colour]
coloursContaining clr rs = [rClr | (rClr, contents) <- rs,
                                   clr `elem` (map fst contents)]

-- Recursively count the bag colours that can eventually contain
-- the specified bag colour
countContains :: Colour -> [Rule] -> [Colour]
countContains clr rs = nub $ countContains' [clr] rs
  where
  countContains' :: [Colour] -> [Rule] -> [Colour]
  countContains' [] _ = []
  countContains' clrs rs = clrs' ++ countContains' clrs' rs
    where
      clrs' = (nub . concat) (map (`coloursContaining` rs) clrs)

-- Count the number of bags required within a specified bag colour
countBagsIn :: Colour -> [Rule] -> Int
countBagsIn clr rs
  | contents == [] = 0
  | otherwise      = (sum $ zipWith (*) cnts (map (+1) (map (`countBagsIn` rs) clrs)))
  where
    rsMap = fromList rs
    contents = rsMap ! clr
    clrs = map fst contents
    cnts = map snd contents

main :: IO()
main
  = do
      s <- readFile "07.txt"
      let rs = map getRule (lines s)

      putStrLn $ "Part 1: " ++ show (length $ countContains "shiny gold" rs)
      putStrLn $ "Part 2: " ++ show (countBagsIn "shiny gold" rs)
