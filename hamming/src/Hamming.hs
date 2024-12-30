module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs == length ys = Just
                                          $ sum
                                          $ zipWith charDistance xs ys
               | otherwise = Nothing
    where
        charDistance x y | x == y = 0
                         | otherwise = 1

