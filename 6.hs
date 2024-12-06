import Data.Array

mkArray xs = let c@(a, b) = (length (head xs), length xs) in
                 array ((1, 1), c) [((x, y), xs !! (y-1) !! (x-1)) | x <- [1..a], y <- [1..b]]
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
rotate (x, y) = (-y, x)
-- inBounds (x1, y1) grid = let (x2, y2) = bounds grid in 
findStart = fst . head . filter ((== '^') . snd) . assocs :: Array (Int, Int) Char -> (Int, Int)
step grid (pos, dir) = let n = pos <+> dir in if grid ! n == '#' then 
main = do
  grid <- mkArray . lines <$> readFile "./6.txt"
  -- print $ mkArray [[1, 2], [3, 4]]
  -- print grid
  print $ findStart grid
