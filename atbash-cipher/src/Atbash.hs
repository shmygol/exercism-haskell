module Atbash (decode, encode) where

import qualified Data.Map as Map
import qualified Data.Char as Char

chanks :: Int -> [a] -> [[a]]
chanks _ [] = []
chanks n xs = take n xs : chanks n (drop n xs)

mapChar :: Map.Map Char Char -> Char -> Maybe Char
mapChar dict c | Char.isDigit c = Just c
               | otherwise = Map.lookup c dict

mapString :: Map.Map Char Char -> String -> String
mapString dict = foldr foldingFunction ""
    where
        foldingFunction :: Char -> String -> String
        foldingFunction c acc = case mapChar dict $ Char.toLower c of
            Nothing -> acc
            Just(mappedChar) -> mappedChar : acc

decode :: String -> String
decode = mapString decodingDict
    where
        decodingDict = Map.fromList $ zip ['z','y'..'a'] ['a'..'z']

encode :: String -> String
encode = unwords . chanks 5 . mapString encodingDict
    where
        encodingDict = Map.fromList $ zip ['a'..'z'] ['z','y'..'a']