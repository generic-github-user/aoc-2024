import Data.List.Split
import Control.Monad

parse :: String -> (Int, [Int])
parse line = let [left, right] = splitOn ": " line in (read left, map read $ words right)
matches (x, y:ys) = x `elem` (foldM (\a b -> [a + b, a * b]) y ys)
main = do
  rows <- map parse . lines <$> readFile "./7.txt"
  print $ sum $ map fst $ filter matches rows
