module MinCaml.Alpha
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

g :: Map.Map Id.T Id.T -> KNormal.T -> MinCaml KNormal.T
g _ KNormal.Unit    = return KNormal.Unit
g _ (KNormal.Int i) = return $ KNormal.Int i

f :: KNormal.T -> MinCaml KNormal.T
f = g Map.empty
