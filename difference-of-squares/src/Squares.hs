module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = (squareOfSum n) - (sumOfSquares n)

squareOfSum :: Integral a => a -> a
squareOfSum n = ((n * (n + 1)) `div` 2) ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (2 * (n ^ 3) + 3 * (n ^ 2) + n) `div` 6
