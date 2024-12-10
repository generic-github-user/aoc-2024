import Data.Array
import Data.Array.IArray ((!?), genArray)
import Data.Set (Set, size, fromList)

mkArray xs = listArray ((1, 1), (length xs, length (head xs))) (concat $ transpose xs)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
(x1, y1) <-> (x2, y2) = (x1 - x2, y1 - y2)
combinations n xs = filter ((== n) . length . nub) (replicate n xs)
