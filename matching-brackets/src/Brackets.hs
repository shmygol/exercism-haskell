module Brackets (arePaired) where

import qualified Data.Map as Map
import Data.Maybe()
import Control.Monad(foldM)

brackets :: Map.Map Char Char
brackets = Map.fromList [('(', ')'), ('[', ']'), ('{', '}')]

brackets_reversed :: Map.Map Char Char
brackets_reversed' = Map.foldrWithKey (flip Map.insert) Map.empty brackets

arePaired :: String -> Bool
arePaired = (Just [] == ) . foldM f []
  where
    f stack x
      | Map.member x brackets = Just (x:stack)
      | (Just matchingOpenBracket) <- Map.lookup x brackets_reversed
        = case stack of
            [] -> Nothing
            (prev:stackTail) -> if prev == matchingOpenBracket
                                then Just stackTail
                                else Nothing
      | otherwise = Just stack