module Pangram (isPangram) where

import Data.Char (toLower)
import qualified Data.Set as Set


isPangram :: String -> Bool
isPangram = Set.isSubsetOf alphabet
            . Set.fromList
            . map toLower
    where
        alphabet = Set.fromList ['a'..'z']
