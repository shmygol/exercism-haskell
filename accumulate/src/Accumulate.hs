module Accumulate (accumulate) where

-- It's basically a map function and can be defined just like this:
-- accumulate :: (a -> b) -> [a] -> [b]
-- accumulate = map

-- But we can also define it recursively:
accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = f x : accumulate f xs
