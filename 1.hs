import Data.List

main = do
  rows <- (map ((\[a, b] -> (a, b)) . (map (read :: String -> Int) . words))
    . lines) <$> readFile "./1.txt"
  let (left, right) = unzip rows
  print (sum $ map abs $ zipWith (-) (sort left) (sort right))
