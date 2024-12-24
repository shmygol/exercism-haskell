module Atbash (decode, encode) where

import qualified Data.Map as Map
import qualified Data.Char as Char

mapChar :: Map.Map Char Char -> Char -> Maybe Char
mapChar dict c | Char.isDigit c = Just c
               | otherwise = Map.lookup c dict

mapStringWith :: Map.Map Char Char -> (String -> Integer -> String) -> String -> String
mapStringWith dict f = mapStringWith' 0 ""
    where
        mapStringWith' :: Integer -> String -> String -> String
        mapStringWith' _ acc [] = acc
        mapStringWith' i acc (x:xs) =
          case mapChar dict (Char.toLower x) of
            Nothing -> mapStringWith' i acc xs
            Just(mappedChar) -> mapStringWith' (i + 1) (acc ++ (f [mappedChar] i)) xs


decode :: String -> String
decode = mapStringWith decodingDict const
    where
        decodingDict = Map.fromList $ zip ['z','y'..'a'] ['a'..'z']

encode :: String -> String
encode = mapStringWith encodingDict prependSeparator
    where
        encodingDict = Map.fromList $ zip ['a'..'z'] ['z','y'..'a']
        prependSeparator :: String -> Integer -> String
        prependSeparator s i | i > 0 && i `mod` 5 == 0 = " " <> s
                             | otherwise = s