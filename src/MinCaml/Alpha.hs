module MinCaml.Alpha
  ( f
  ) where

import           Control.Monad   (liftM2)
import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

find :: Id.T -> Map.Map Id.T Id.T -> Id.T
find x = Map.findWithDefault x x

g :: Map.Map Id.T Id.T -> KNormal.T -> MinCaml KNormal.T
g _ KNormal.Unit = return KNormal.Unit
g _ (KNormal.Int i) = return $ KNormal.Int i
g env (KNormal.Add x y) = return $ KNormal.Add (find x env) (find y env)
g env (KNormal.Sub x y) = return $ KNormal.Sub (find x env) (find y env)
g env (KNormal.IfEq x y e1 e2) = liftM2 (KNormal.IfEq (find x env) (find y env)) (g env e1) (g env e2)
g env (KNormal.IfLe x y e1 e2) = liftM2 (KNormal.IfLe (find x env) (find y env)) (g env e1) (g env e2)
g env (KNormal.Let (x, t) e1 e2) = do
  x' <- genId x
  liftM2 (KNormal.Let (x', t)) (g env e1) (g env e2)

f :: KNormal.T -> MinCaml KNormal.T
f = g Map.empty
