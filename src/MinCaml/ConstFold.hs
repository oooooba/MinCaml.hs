module MinCaml.ConstFold
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

memi :: Id.T -> Map.Map Id.T KNormal.T -> Bool
memi x env =
  case Map.lookup x env of
    Just (KNormal.Int _) -> True
    _                    -> False

findi :: Id.T -> Map.Map Id.T KNormal.T -> Int
findi x env =
  case Map.lookup x env of
    Just (KNormal.Int i) -> i
    _                    -> error $ "findi: " ++ x

g :: Map.Map Id.T KNormal.T -> KNormal.T -> KNormal.T
g env (KNormal.Neg x)
  | memi x env = KNormal.Int $ negate $ findi x env
g env (KNormal.Add x y)
  | memi x env && memi y env = KNormal.Int $ findi x env + findi y env
g env (KNormal.Sub x y)
  | memi x env && memi y env = KNormal.Int $ findi x env - findi y env
g env (KNormal.Var x)
  | memi x env = KNormal.Int $ findi x env
g env (KNormal.Let (x, t) e1 e2) =
  let e1' = g env e1
      e2' = g (Map.insert x e1' env) e2
  in KNormal.Let (x, t) e1' e2'
g env e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g Map.empty e
