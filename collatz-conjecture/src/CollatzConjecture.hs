module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = recursiveCollatz 0
  where
    recursiveCollatz counter x
      | x < 1 = Nothing
      | x == 1 = Just counter
      | otherwise = recursiveCollatz (counter + 1) (if even x then (x `div` 2) else (x * 3 + 1))
