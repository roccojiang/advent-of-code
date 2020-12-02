checkPwds :: [[String]] -> [String]
checkPwds [] = []
checkPwds (l : ls)
  | valid     = pwd : checkPwds ls
  | otherwise = checkPwds ls
  where
    (n : s : pwd : []) = l
    (p1', '-' : p2')   = break ('-'==) n
    (p1, p2)           = ((read p1' :: Int) - 1, (read p2' :: Int) - 1)
    (c1, c2)           = (pwd !! p1, pwd !! p2)
    c                  = head s
    valid              = (length $ filter (c==) [c1, c2]) == 1

main :: IO()
main
  = do
      s <- readFile "02.txt"
      let ls   = map words (lines s)
      print $ length (checkPwds ls)
