import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.Map (fromList, (!))

type Field = (String, String)
type Fields = [Field]

-- Combine input split by lines into their own separated passport items
combine :: [String] -> [[String]]
combine [] = []
combine (x : xs)
  = combine' x xs : combine (drop 1 $ dropWhile (""/=) xs)
  where
    combine' :: String -> [String] -> [String]
    combine' x xs = x : takeWhile (""/=) xs

-- Split string by ':' delimiter
split :: String -> Field
split s = (s', s'')
  where
    (s', ':' : s'') = break (':'==) s

-- Checks passport validity for Part 1
isValid1 :: Fields -> Bool
isValid1 fs = case (length fs) of
  8 -> True
  7 -> not $ elem "cid" (map fst fs)
  _ -> False

-- Helper function to check if an Ord is between two given values
isBetween :: Ord a => a -> a -> a -> Bool
isBetween alow ahigh a
  = a >= alow && a <= ahigh

byrValid :: Int -> Bool
byrValid byr = isBetween 1920 2002 byr

iyrValid :: Int -> Bool
iyrValid iyr = isBetween 2010 2020 iyr

eyrValid :: Int -> Bool
eyrValid eyr = isBetween 2020 2030 eyr

hgtValid :: String -> Bool
hgtValid hgt = case hgtUnit of
  "cm" -> isBetween 150 193 hgtNum
  "in" -> isBetween 59 76 hgtNum
  _    -> False
  where
    hgtUnit = reverse $ take 2 $ reverse hgt
    hgtNum  = read (reverse $ drop 2 $ reverse hgt) :: Int

hclValid :: String -> Bool
hclValid hcl
  | length hcl == 7 && prefix == "#" = all (==True) $ map isValidChar code
  | otherwise                        = False
  where
    prefix = take 1 hcl
    code   = drop 1 hcl
    isValidChar :: Char -> Bool
    isValidChar c = isBetween 'a' 'f' c || isBetween '0' '9' c

eclValid :: String -> Bool
eclValid ecl = or (map (== ecl) ["amb","blu","brn","gry","grn","hzl","oth"])

pidValid :: String -> Bool
pidValid pid = length pid == 9 && isJust (readMaybe pid :: Maybe Int)

-- Checks passport validity for Part 1
isValid2 :: Fields -> Bool
isValid2 fs
  | isValid1 fs = and [byrValid byr,
                       iyrValid iyr,
                       eyrValid eyr,
                       hgtValid hgt,
                       hclValid hcl,
                       eclValid ecl,
                       pidValid pid]
  | otherwise   = False
  where
    fs'      = fromList fs
    byr      = read (fs' ! "byr") :: Int
    iyr      = read (fs' ! "iyr") :: Int
    eyr      = read (fs' ! "eyr") :: Int
    hgt      = fs' ! "hgt"
    hcl      = fs' ! "hcl"
    ecl      = fs' ! "ecl"
    pid      = fs' ! "pid"

main :: IO()
main
  = do
      s <- readFile "04.txt"
      let pps = (map . map) split $ map (words . unwords) $ (combine . lines) s
          part1 = length $ filter (==True) $ map isValid1 pps
          part2 = length $ filter (==True) $ map isValid2 pps
      putStrLn $ "Part 1: " ++ show part1
      putStrLn $ "Part 2: " ++ show part2
