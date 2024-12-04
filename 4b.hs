import Data.List

(<+>) = zipWith (+)
(<!!>) xs [i, j] = ((!! i) . (!! j)) xs
indices xs = sequence [[1..length (head xs) - 2], [1..length xs - 2]]
corners xs i = map ((xs <!!>) . (i <+>)) [[-1,-1],[-1,1],[1,1],[1,-1]]
main = do
  grid <- lines <$> readFile "./4.txt"
  print $ length $ filter (\i -> let c = corners grid i in grid <!!> i == 'A' &&
      c `elem` cycles "MMSS") (indices grid)
        where cycles xs = tail $ zipWith (++) (tails xs) (inits xs)
