module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import Control.Monad (foldM)
import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

fromChar :: Char -> Either String Nucleotide
fromChar 'A' = Right A
fromChar 'C' = Right C
fromChar 'G' = Right G
fromChar 'T' = Right T
fromChar _ = Left "Invalid nucleotide"

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldM count Map.empty . map fromChar
  where
    count acc (Right n) = Right $ Map.insertWith (+) n 1 acc
    count _ (Left e) = Left e
