import Text.Regex.Posix
import Data.List

r = "do\\(\\)|don't\\(\\)|mul\\(([0-9]+),([0-9]+)\\)"
process n on rest = case rest =~ r :: (String, String, String, [String]) of
  (before, x, after, xs)
    | x == "do()" -> process n True after
    | x == "don't()" -> process n False after
    | isPrefixOf "mul" x -> process (n + if on then
                                    (read (xs !! 0) * read (xs !! 1)) else 0) on after
  _ -> n

main = do
  text <- readFile "./3.txt"
  print $ process 0 True text


