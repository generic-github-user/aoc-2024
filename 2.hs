diff [] = []
diff xs = zipWith (-) (tail xs) xs
main = do
  rows <- map (map read . words) . lines <$> readFile "./2.txt" :: IO [[Int]]
  print $ length $ filter (\x -> let d = diff x in
    (all (< 0) d || all (> 0) d) && all ((`elem` [1..3]) . abs) d) rows


