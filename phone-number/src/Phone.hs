module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number rawInput = case clean of
    xs@(x1:_:_:x4:_:_:_:_:_:_:rest) | rest == [] && isValidLead x1 && isValidLead x4 -> Just xs
    _ -> Nothing

    where
        clean = case filter isDigit rawInput of
            ('1':xs) -> xs
            xs -> xs

        isValidLead = flip elem ['2'..'9']
