module Pangram (isPangram) where

import qualified Data.Set as Set
import Data.Char
import Data.List

alphabet = Set.fromList ['a'..'z']

isPangram :: String -> Bool
isPangram text = Set.null $ Set.difference alphabet textSet
    where
        textSet = Set.fromList $ fmap toLower $ filter (/= ' ') text