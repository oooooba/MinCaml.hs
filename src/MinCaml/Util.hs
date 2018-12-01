module MinCaml.Util where

import qualified Data.Map as Map

addList :: Ord k => [(k, v)] -> Map.Map k v -> Map.Map k v
addList xys env = foldl (\e (x, y) -> Map.insert x y e) env xys

addList2 :: Ord k => [k] -> [v] -> Map.Map k v -> Map.Map k v
addList2 xs ys = addList (zip xs ys)
