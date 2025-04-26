module Brackets (arePaired) where

import qualified Data.Map as Map
import Data.Maybe()
import Control.Monad(foldM)

brackets :: Map.Map Char Char
brackets = Map.fromList [('(', ')'), ('[', ']'), ('{', '}')]

brackets' :: Map.Map Char Char
brackets' = Map.foldrWithKey (\k v m -> Map.insert v k m) Map.empty brackets

arePaired :: String -> Bool
arePaired = (Just [] == ) . foldM f []
  where
    f stack x
      | Map.member x brackets = Just (x:stack)
      | (Just matchingOpenBracket) <- Map.lookup x brackets'
        = case stack of
            [] -> Nothing
            (prev:stackTail) -> if prev == matchingOpenBracket
                                then Just stackTail
                                else Nothing
      | otherwise = Just stack