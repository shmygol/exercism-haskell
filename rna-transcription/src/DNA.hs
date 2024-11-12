module DNA (toRNA) where

import qualified Data.Map as Map

rules :: Map.Map Char Char
rules = Map.fromList [('G', 'C'),
                      ('C', 'G'),
                      ('T', 'A'),
                      ('A', 'U')]

transcript :: Char -> Either Char Char
transcript char = maybe (Left char) Right (Map.lookup char rules)

toRNA :: String -> Either Char String
toRNA = traverse transcript
