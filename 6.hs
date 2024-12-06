import Data.Array
import Data.Array.IArray ((!?), genArray)
import Data.Set (Set, size, fromList)
import Data.List

type Index = (Int, Int)
type Dir = Index
mkArray xs = listArray ((1, 1), (length xs, length (head xs))) (concat $ transpose xs)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
rotate (x, y) = (-y, x)
findStart = fst . head . filter ((== '^') . snd) . assocs :: Array Index Char -> Index
repeatedly f x = case f x of Nothing -> []; Just y -> y : repeatedly f y

step :: Array Index Char -> (Index, Dir) -> Maybe (Index, Dir)
step grid (pos, dir) = let n = pos <+> dir in do
  v <- grid !? n
  return $ if v == '#' then let r = rotate dir in (pos <+> r, r) else (n, dir)

main = do
  grid <- mkArray . lines <$> readFile "./6.txt"
  let start = ((findStart grid), (0, -1))
  let trace = start : repeatedly (step grid) start
  print $ size (fromList (map fst trace))
  print $ (genArray (bounds grid) (gen grid trace)) ! (5, 5)
