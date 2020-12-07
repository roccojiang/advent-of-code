import Text.Read (readMaybe)
import Data.List (nub)

type Colour   = String
type Contents = [(Int, Colour)]
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
        Just x  -> (x, clr) : getContents next
        Nothing -> []
        where
          clr = (unwords . take 2) ss
          next = drop 3 ss

-- Count the bag colours that contain the specified bag colour
coloursContaining :: Colour -> [Rule] -> [Colour]
coloursContaining clr rs = [rClr | (rClr, contents) <- rs,
                                         clr `elem` (map snd contents)]

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

contains :: Colour -> [Rule] -> [Rule]
contains clr rs = [r | r@(_, contents) <- rs,
                       let clrs = coloursContaining clr rs,
                       or $ map (`elem` (map snd contents)) clrs]

main :: IO()
main
  = do
      s <- readFile "07.txt"
      let rs = map getRule (lines s)

          test = map getRule ["light red bags contain 1 bright white bag, 2 muted yellow bags.","dark orange bags contain 3 bright white bags, 4 muted yellow bags.","bright white bags contain 1 shiny gold bag.","muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.","shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.","dark olive bags contain 3 faded blue bags, 4 dotted black bags.","vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.","faded blue bags contain no other bags.","dotted black bags contain no other bags."]

      putStrLn $ "Part 1: " ++ show (length $ countContains "shiny gold" rs)
