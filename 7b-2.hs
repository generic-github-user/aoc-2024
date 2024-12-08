import Data.List
import Data.List.Split
import Control.Monad

parse :: String -> (Int, [Int])
parse line = let [left, right] = splitOn ": " line in (read left, map read $ words right)
matches (x, y:ys) = y `elem` (foldM (\a b -> let (as, bs) = (show a, show b) in
   if a < 0 then [] else [a - b]
    ++ (if a `rem` b == 0 then [a `div` b] else [])
    ++ (if length as > length bs && isSuffixOf bs as then
         [read $ take (length as - length bs) as] else [])) x (reverse ys))
main = do
  rows <- map parse . lines <$> readFile "./7.txt"
  print $ sum $ map fst $ filter matches rows
