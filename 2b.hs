import Data.List

diff [] = []
diff xs = zipWith (-) (tail xs) xs
alts xs = zipWith mappend (inits xs) (tail (tails xs)) ++ [xs]
main = do
  rows <- map (map read . words) . lines <$> readFile "./2.txt" :: IO [[Int]]
  print $ length $ filter (any (\x -> let d = diff x in
               (all (< 0) d || all (> 0) d) && all ((`elem` [1..3]) . abs) d) . alts) rows
