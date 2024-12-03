import Text.Regex.Posix

main = do
  text <- readFile "./3.txt"
  let r = "mul\\(([0-9]+),([0-9]+)\\)"
  let matches = getAllTextMatches $ text =~ r :: [String]
  let parseMatch x = (let (_, _, _, sub) = x =~ r :: (String, String, String, [String])
                       in map read sub :: [Int])
  print $ sum $ map (product . parseMatch) matches
