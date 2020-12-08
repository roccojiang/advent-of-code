type Instr = (Int, (String, Int))

-- Get instructions from input
getInstrs :: [String] -> [Instr]
getInstrs ls = zip [0..] (getInstrs' ls)
  where
  getInstrs' :: [String] -> [(String, Int)]
  getInstrs' [] = []
  getInstrs' (l : ls)
    = (op, arg) : getInstrs' ls
    where
      (op : arg' : []) = words l
      arg = case (head arg') of
              '+' -> read (tail arg') :: Int
              _   -> read arg' :: Int

-- Returns the value of the accumulator before the program hits an infinite loop
parseInstrs :: [Instr] -> Int
parseInstrs is
  = parseInstrs' is is [] 0
  where
    parseInstrs' :: [Instr] -> [Instr] -> [Int] -> Int -> Int
    parseInstrs' [] _ _ acc = acc
    parseInstrs' (i : is) allIs prevLines acc
      | curLine `elem` prevLines = acc
      | otherwise = case curOp of
        "nop" -> parseInstrs' is allIs (curLine : prevLines) acc
        "acc" -> parseInstrs' is allIs (curLine : prevLines) (acc + curArg)
        "jmp" -> case (curArg >= 0) of
          True  -> parseInstrs' (drop (curArg - 1) is) allIs (curLine : prevLines) acc
          False -> parseInstrs' (drop (curLine + curArg) allIs) allIs (curLine : prevLines) acc
        where
          curLine = fst i
          curOp   = fst (snd i)
          curArg  = snd (snd i)

main :: IO()
main
  = do
      s <- readFile "08.txt"
      let code = getInstrs (lines s)

          test' = ["nop +0","acc +1","jmp +4","acc +3","jmp -3","acc -99","acc +1","jmp -4","acc +6"]
          test  = getInstrs test'

      putStrLn $ "Part 1: " ++ show (parseInstrs code)
