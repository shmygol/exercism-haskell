module SumOfMultiples (sumOfMultiples) where

-- import Prelude
-- import Data.Foldable (sum)
-- import Data.Set as Set
-- import Data.List.Lazy as List
import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples items level = sum
                             $ Set.unions
                             $ Set.map (energyMultiplies level)
                             $ Set.fromList items



energyMultiplies :: Integer -> Integer -> Set.Set Integer
energyMultiplies _     0    = Set.empty
energyMultiplies level item = Set.fromList
                              $ takeWhile (level >)
                              $ iterate (item +) item