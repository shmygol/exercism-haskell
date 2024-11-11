module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor input | null trimmedInput = "Fine. Be that way!"
                  | isYelling && isQuestion = "Calm down, I know what I'm doing!"
                  | isYelling = "Whoa, chill out!"
                  | isQuestion = "Sure."
                  | otherwise = "Whatever."
            where
                trimmedInput = filter (not . isSpace) input
                isYelling = any isUpper trimmedInput && all (not . isLower) trimmedInput
                isQuestion = last trimmedInput == '?'

