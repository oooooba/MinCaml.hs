module MinCaml.Elim
  ( f
  ) where

import qualified Data.Set        as Set

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

effect :: KNormal.T -> Bool
effect (KNormal.Let _ e1 e2) = effect e1 || effect e2
effect _                     = False

g :: KNormal.T -> KNormal.T
g (KNormal.Let (x, t) e1 e2) =
  let e1' = g e1
      e2' = g e2
  in if effect e1' || Set.member x (KNormal.fv e2')
       then KNormal.Let (x, t) e1' e2'
       else e2'
g e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g e
