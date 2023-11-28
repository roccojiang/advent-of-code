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

-- Returns a Just Int for the accumulator if the instructions finish;
-- otherwise Nothing if it results in infinite loop
checkFinish :: [Instr] -> Maybe Int
checkFinish is
  = parseInstrs' is is [] 0
  where
    parseInstrs' :: [Instr] -> [Instr] -> [Int] -> Int -> Maybe Int
    parseInstrs' [] _ _ acc = Just acc
    parseInstrs' (i : is) allIs prevLines acc
      | curLine `elem` prevLines = Nothing
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

-- Returns the traceback of which lines were run up till infinite loop
traceback :: [Instr] -> [Int]
traceback is
  = parseInstrs' is is [] 0
  where
    parseInstrs' :: [Instr] -> [Instr] -> [Int] -> Int -> [Int]
    parseInstrs' [] _ prevLines _ = prevLines
    parseInstrs' (i : is) allIs prevLines acc
      | curLine `elem` prevLines = prevLines
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

-- Replace the line causing an infinite loop and return the accumulator
replaceLine :: [Instr] -> [Int] -> Int
replaceLine is (n : ns) = case curOp of
  "nop" -> case (checkFinish (is' ++ [(n, ("jmp", curArg))] ++ is'')) of
    Just acc  -> acc
    Nothing -> replaceLine is ns
  "jmp" -> case (checkFinish (is' ++ [(n, ("nop", curArg))] ++ is'')) of
    Just acc  -> acc
    Nothing -> replaceLine is ns
  _     -> replaceLine is ns
  where
    curInstr = snd (is !! n)
    curOp    = fst curInstr
    curArg   = snd curInstr
    (is', _ : is'') = splitAt n is

main :: IO()
main
  = do
      s <- readFile "08.txt"
      let code = getInstrs (lines s)

      putStrLn $ "Part 1: " ++ show (parseInstrs code)
      putStrLn $ "Part 2: " ++ show (replaceLine code (traceback code))
