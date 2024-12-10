import Data.List
import Data.Tuple
import Data.Array
import Data.Array.IArray ((!?), genArray)
import Data.Set (Set, size, fromList, unions, empty)
import qualified Data.Set as S
import qualified Data.Map as M

mkArray xs = listArray ((1, 1), (length xs, length (head xs))) (concat $ transpose xs)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) <-> (x2, y2) = (x1 - x2, y1 - y2)
findAll x = map fst . filter ((== x) . snd)
combinations xs = concat $ map (zipWith (,) xs) (tail (tails xs))
antinodes m letter = case (M.lookup letter m) of
  Just v -> (fromList $ concatMap (\(a, b) ->
    let d = b <-> a in [a <-> d, b <+> d]) (combinations v))
  Nothing -> empty

main = do
  grid <- mkArray . lines <$> readFile "./8.txt"
  let m = M.fromListWith (++) (map (fmap pure . swap) (filter ((/= '.') . snd) $ assocs grid))
  let allAntinodes = S.filter (inRange (bounds grid)) (unions (map (antinodes m) (M.keys m)))
  print $ size allAntinodes
