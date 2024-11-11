module Bob (responseFor) where

-- import Prelude
-- import Data.Maybe
-- import Data.String.CodeUnits (toCharArray) as String
-- import Data.String.Common (trim) as String
-- import Data.Enum (enumFromTo)
-- import Data.Array.NonEmpty (NonEmptyArray, intersect, last, fromArray) as NonEmptyA
-- import Data.Array (null)
import qualified Data.Text as Text
import qualified Data.Char as Char

responseFor :: String -> String
responseFor = responseFor' . Text.strip . Text.pack
            where
                isYelling :: Text.Text -> Bool
                isYelling inputText = Text.any Char.isUpper inputText && Text.all (not . Char.isLower) inputText

                isQuestion :: Text.Text -> Bool
                isQuestion inputText = Text.last inputText == '?'

                isYelledQuestion :: Text.Text -> Bool
                isYelledQuestion inputChars = isYelling inputChars && isQuestion inputChars

                responseFor' :: Text.Text -> String
                responseFor' t | Text.null t = "Fine. Be that way!"
                               | isYelledQuestion t = "Calm down, I know what I'm doing!"
                               | isYelling t = "Whoa, chill out!"
                               | isQuestion t = "Sure."
                               | otherwise = "Whatever."


