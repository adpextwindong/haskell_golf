module Dups where

import Data.Char
import Data.Map.Strict

mapDups :: Map Char Int -> Char -> Char
mapDups m c = case Data.Map.Strict.lookup c m of
                        Nothing -> '('
                        Just n -> if n == 1
                                  then '('
                                  else ')'

duplicateEncode :: String -> String
duplicateEncode str = Prelude.map (mapDups unique_map) str
  where
    base_list = Prelude.map (\c -> (Data.Char.toLower c,1)) str
    unique_map = Data.Map.Strict.fromListWith (+) base_list
