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

memt :: Id.T -> Map.Map Id.T KNormal.T -> Bool
memt x env =
  case Map.lookup x env of
    Just (KNormal.Tuple _) -> True
    _                      -> False

findi :: Id.T -> Map.Map Id.T KNormal.T -> Int
findi x env =
  case Map.lookup x env of
    Just (KNormal.Int i) -> i
    _                    -> error $ "findi: " ++ x

findt :: Id.T -> Map.Map Id.T KNormal.T -> [Id.T]
findt x env =
  case Map.lookup x env of
    Just (KNormal.Tuple ys) -> ys
    _                       -> error $ "findt: " ++ x

g :: Map.Map Id.T KNormal.T -> KNormal.T -> KNormal.T
g env (KNormal.Neg x)
  | memi x env = KNormal.Int $ negate $ findi x env
g env (KNormal.Add x y)
  | memi x env && memi y env = KNormal.Int $ findi x env + findi y env
g env (KNormal.Sub x y)
  | memi x env && memi y env = KNormal.Int $ findi x env - findi y env
g env (KNormal.Var x)
  | memi x env = KNormal.Int $ findi x env
g env (KNormal.IfEq x y e1 e2)
  | memi x env && memi y env =
    if findi x env == findi y env
      then g env e1
      else g env e2
g env (KNormal.IfEq x y e1 e2) = KNormal.IfEq x y (g env e1) $ g env e2
g env (KNormal.IfLe x y e1 e2)
  | memi x env && memi y env =
    if findi x env <= findi y env
      then g env e1
      else g env e2
g env (KNormal.IfLe x y e1 e2) = KNormal.IfLe x y (g env e1) $ g env e2
g env (KNormal.Let (x, t) e1 e2) =
  let e1' = g env e1
      e2' = g (Map.insert x e1' env) e2
  in KNormal.Let (x, t) e1' e2'
g env (KNormal.LetRec (KNormal.Fundef xt yts e1) e2) = KNormal.LetRec (KNormal.Fundef xt yts $ g env e1) $ g env e2
g env (KNormal.LetTuple xts y e)
  | memt y env = foldl (\e' (xt, z) -> KNormal.Let xt (KNormal.Var z) e') (g env e) $ zip xts (findt y env)
g env (KNormal.LetTuple xts y e) = KNormal.LetTuple xts y $ g env e
g env e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g Map.empty e
