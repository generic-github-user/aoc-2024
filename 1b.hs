import Data.List

main = do
  rows <- (map ((\[a, b] -> (a, b)) . (map (read :: String -> Int) . words))
    . lines) <$> readFile "./1.txt"
  let (left, right) = unzip rows
  print $ sum [a | a <- left, b <- right, a == b]
