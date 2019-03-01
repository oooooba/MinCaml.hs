module MinCaml.Elim
  ( f
  ) where

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

g :: KNormal.T -> KNormal.T
g e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g e
