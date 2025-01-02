module Raindrops (convert) where

convert :: Int -> String
convert inputNumber = case concatMap ($ inputNumber) [pling, plang, plong] of
    "" -> show inputNumber
    drops -> drops

    where
        plxng (divisor, result) n | n `rem` divisor == 0 = result
                                  | otherwise = ""
        pling = plxng (3, "Pling")
        plang = plxng (5, "Plang")
        plong = plxng (7, "Plong")
