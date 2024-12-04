import Data.List
import Data.Function (on)

-- TODO: alternative approach where we just enumerate valid starting coords + directions
z = [[x*n+1..(x+1)*n] | x <- [0..n-1]] where n = 3
groupWith f xs = groupBy ((==) `on` f) (sortOn f xs)
index2D [i, j] = (!! i) . (!! j)
-- can this be made point-free?
-- indexND = foldl' (\a b -> a . (!! b)) id
indices x y = groupWith sum $ sequence [[0..x-1], [0..y-1]] :: [[[Int]]]
diagonals xs = map (map $ flip index2D $ xs) (indices (length $ head xs) (length xs))
occurrences s = length . filter (s `isPrefixOf`) . tails
getSpans y = y ++ transpose y ++ diagonals y ++ (diagonals $ reverse y)
main = do
  grid <- lines <$> readFile "./4.txt"
  print $ indices 3 3
  print $ diagonals z
  print $ z
  let spans_a = getSpans grid
  let spans_b = spans_a ++ map reverse spans_a
  -- print spans_b
  print $ sum $ map (occurrences "XMAS") spans_b
