checkPwds :: [[String]] -> [String]
checkPwds [] = []
checkPwds (l : ls)
  | valid     = pwd : checkPwds ls
  | otherwise = checkPwds ls
  where
    (n : s : pwd : []) = l
    (min, '-' : max)   = break ('-'==) n
    c                  = head s
    cnt                = length $ filter (c==) pwd
    valid              = cnt >= (read min :: Int) && cnt <= (read max :: Int)

main :: IO()
main
  = do
      s <- readFile "02.txt"
      let ls   = map words (lines s)
      print $ length (checkPwds ls)
