main :: IO()
main
  = do
      content <- readFile "01.txt"
      let nums = map (read :: String -> Int) (lines content)
      putStrLn $ show nums