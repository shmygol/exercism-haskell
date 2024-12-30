module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = (squareOfSum n) - (sumOfSquares n)

squareOfSum :: Integral a => a -> a
squareOfSum n = round $ (^ 2) $ (n' / 2) * (n' + 1)
    where
        n' = fromIntegral n

sumOfSquares :: Integral a => a -> a
sumOfSquares n = round $ (n' ^ 3) / 3.0 + (n' ^ 2) / 2.0 + n' / 6.0
    where
        n' = fromIntegral n
