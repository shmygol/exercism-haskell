module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter | elem normalizedLetter "AEIOULNRST" = 1
                   | elem normalizedLetter "DG" = 2
                   | elem normalizedLetter "BCMP" = 3
                   | elem normalizedLetter "FHVWY" = 4
                   | elem normalizedLetter "K" = 5
                   | elem normalizedLetter "JX" = 8
                   | elem normalizedLetter "QZ" = 10
                   | otherwise = 0
    where
        normalizedLetter = toUpper letter

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter . map toUpper
