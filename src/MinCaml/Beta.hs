module MinCaml.Beta
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

g :: Map.Map Id.T KNormal.T -> KNormal.T -> KNormal.T
g env KNormal.Unit    = KNormal.Unit
g env (KNormal.Int i) = KNormal.Int i

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g Map.empty e
