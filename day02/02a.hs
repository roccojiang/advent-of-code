type PwdPolicy = (Int, Int, Char, String)

-- Parse input into a nice format.
parse :: [[String]] -> [PwdPolicy]
parse [] = []
parse (l : ls)
  = (min, max, c, pwd) : parse ls
  where
    (n : s : pwd : []) = l
    (min', '-' : max') = break ('-'==) n
    (min, max)         = (read min' :: Int, read max' :: Int)
    c                  = head s

-- Check if a password is valid.
checkPwd :: PwdPolicy -> Bool
checkPwd (min, max, c, pwd) = cnt >= min && cnt <= max
  where
    cnt = length $ filter (c==) pwd

-- Count the number of valid passwords, from the raw input string.
countPwds :: String -> Int
countPwds = length . filter checkPwd . parse . map words . lines

main :: IO()
main
  = do
      s <- readFile "02.txt"
      print $ countPwds s
