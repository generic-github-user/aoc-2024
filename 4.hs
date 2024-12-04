import Data.List
import Data.Function (on)

groupWith f xs = groupBy ((==) `on` f) (sortOn f xs)
index2D [i, j] = (!! i) . (!! j)
indices x y = groupWith sum $ sequence [[0..x-1], [0..y-1]] :: [[[Int]]]
diagonals xs = map (map $ flip index2D $ xs) (indices (length $ head xs) (length xs))
occurrences s = length . filter (s `isPrefixOf`) . tails
getSpans y = y ++ transpose y ++ diagonals y ++ (diagonals $ reverse y)
main = do
  grid <- lines <$> readFile "./4.txt"
  let spans_a = getSpans grid
  let spans_b = spans_a ++ map reverse spans_a
  print $ sum $ map (occurrences "XMAS") spans_b


