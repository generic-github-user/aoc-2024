instance Num a => Num [a] where
  (+) = zipWith (+)
  (*) = zipWith (*)
  abs = map abs
  negate = map negate
  fromInteger x = [fromInteger x]
  signum = map signum
