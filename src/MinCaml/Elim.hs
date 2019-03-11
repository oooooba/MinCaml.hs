module MinCaml.Elim
  ( f
  ) where

import qualified Data.Set        as Set

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

effect :: KNormal.T -> Bool
effect (KNormal.IfEq _ _ e1 e2) = effect e1 || effect e2
effect (KNormal.IfLe _ _ e1 e2) = effect e1 || effect e2
effect (KNormal.Let _ e1 e2)    = effect e1 || effect e2
effect (KNormal.LetRec _ e)     = effect e
effect _                        = False

g :: KNormal.T -> KNormal.T
g (KNormal.IfEq x y e1 e2) = KNormal.IfEq x y (g e1) $ g e2
g (KNormal.IfLe x y e1 e2) = KNormal.IfLe x y (g e1) $ g e2
g (KNormal.Let (x, t) e1 e2) =
  let e1' = g e1
      e2' = g e2
  in if effect e1' || Set.member x (KNormal.fv e2')
       then KNormal.Let (x, t) e1' e2'
       else e2'
g (KNormal.LetRec (KNormal.Fundef (x, t) yts e1) e2) =
  let e2' = g e2
  in if Set.member x (KNormal.fv e2)
       then KNormal.LetRec (KNormal.Fundef (x, t) yts $ g e1) e2'
       else e2'
g e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g e
