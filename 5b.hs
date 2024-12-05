import Data.List
import Data.List.Split
import Data.Set (Set, fromList, member)

parse :: [String] -> (Set (Int, Int), [[Int]])
parse s = (\[left, right] -> (fromList $ map ((\[a, b] -> (a, b)) . map read . splitOn "|") left :: Set (Int, Int),
                             map (map read . splitOn ",") right)) (splitOn [""] s)
getOrder order x y = if member (x, y) order then LT else if member (y, x) order then GT else EQ
middle xs = xs !! (length xs `div` 2)
main = do
  (order, updates) <- parse . lines <$> readFile "./5.txt"
  let s = sortBy (getOrder order)
  print $ sum $ map middle $ map s $ filter (\u -> s u /= u) updates


