module Isogram (isIsogram) where

import Data.Char (toLower, isAlpha)

isIsogram :: String -> Bool
isIsogram = isIsogram' . fmap toLower . filter isAlpha
    where
        isIsogram' [] = True
        isIsogram' (x:xs) = x `notElem` xs && isIsogram' xs
