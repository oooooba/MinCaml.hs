module MinCaml.ConstFold
  ( f
  ) where

import           Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map               as Map

import           MinCaml.Global
import qualified MinCaml.Id             as Id
import qualified MinCaml.KNormal        as KNormal

type MinCamlConstFold a = Identity a

g :: Map.Map Id.T KNormal.T -> KNormal.T -> MinCamlConstFold KNormal.T
g env = return

run :: MinCamlConstFold a -> a
run = runIdentity

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ run $ g Map.empty e
