module MinCaml.Inline
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

g :: Int -> Map.Map Id.T KNormal.T -> KNormal.T -> KNormal.T
g threshold env e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = do
  let threshold = 0 -- ToDo: fix to read from MinCaml monad
  return $ g threshold Map.empty e
