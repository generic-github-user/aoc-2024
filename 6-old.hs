import Data.Array
import Data.Array.IArray ((!?))
import Data.Set (Set, size, fromList)
import Data.List

-- mkArray xs = let c@(a, b) = (length (head xs), length xs) in
                 -- array ((1, 1), c) [((x, y), xs !! (y-1) !! (x-1)) | x <- [1..a], y <- [1..b]]
type Index = (Int, Int)
type Dir = Index
mkArray xs = listArray ((1, 1), (length xs, length (head xs))) (concat $ transpose xs)
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)
rotate (x, y) = (-y, x)
-- inBounds (x1, y1) grid = let (x2, y2) = bounds grid in 
findStart = fst . head . filter ((== '^') . snd) . assocs :: Array Index Char -> Index
-- borrowed from https://hackage.haskell.org/package/HaXml-1.25.13/docs/src/Text.XML.HaXml.Schema.TypeConversion.html#repeatedly
repeatedly f x = case f x of Nothing -> []; Just y -> y : repeatedly f y

step :: Array Index Char -> (Index, Dir) -> Maybe (Index, Dir)
step grid (pos, dir) = let n = pos <+> dir in do
  v <- grid !? n
  return $ if v == '#' then let r = rotate dir in (pos <+> r, r) else (n, dir)

-- arrayToStringWithNewlines arr = 
  -- let ((rStart, cStart), (rEnd, cEnd)) = bounds arr
      -- rows = [[arr ! (r, c) | c <- [cStart..cEnd]] | r <- [rStart..rEnd]]
  -- in intercalate "\n" (map concat rows)
arrayToStringWithNewlines :: Array (Int, Int) Char -> String
arrayToStringWithNewlines arr =
  let ((rStart, cStart), (rEnd, cEnd)) = bounds arr
      rows = [[arr ! (r, c) | c <- [cStart..cEnd]] | r <- [rStart..rEnd]]
  in unlines rows
gen :: Array Index Char -> [(Index, Dir)] -> Index -> Char
gen grid trace (x, y) = if (x, y) `elem` (map fst trace) then 'X' else grid ! (x, y)

main = do
  grid <- mkArray . lines <$> readFile "./6-test.txt"
  -- print $ mkArray [[1, 2], [3, 4]]
  print grid
  print $ findStart grid
  let trace = repeatedly (step grid) ((findStart grid), (0, -1))
  print trace
  print $ size (fromList (map fst trace))
