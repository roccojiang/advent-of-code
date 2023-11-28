type PwdPolicy = (Int, Int, Char, String)

-- Parse input into a nice format.
parse :: [[String]] -> [PwdPolicy]
parse [] = []
parse (l : ls)
  = (n1, n2, c, pwd) : parse ls
  where
    (n : s : pwd : []) = l
    (n1', '-' : n2')   = break ('-'==) n
    (n1, n2)           = (read n1' :: Int, read n2' :: Int)
    c                  = head s

-- Check if a password is valid, per the rules of part 1.
checkPwd1 :: PwdPolicy -> Bool
checkPwd1 (min, max, c, pwd) = cnt >= min && cnt <= max
  where
    cnt = length $ filter (c==) pwd

-- Check if a password is valid, per the rules of part 2.
checkPwd2 :: PwdPolicy -> Bool
checkPwd2 (p1, p2, c, pwd) = (length $ filter (c==) [c1, c2]) == 1
  where
    (c1, c2) = (pwd !! (p1 - 1), pwd !! (p2 - 1))

-- Count the number of valid passwords, from the raw input string.
countPwds :: String -> (PwdPolicy -> Bool) -> Int
countPwds = (length .) . flip filter . parse . map words . lines

main :: IO()
main
  = do
      s <- readFile "02.txt"
      putStrLn $ "Part 1: " ++ show (countPwds s checkPwd1)
      putStrLn $ "Part 2: " ++ show (countPwds s checkPwd2)
