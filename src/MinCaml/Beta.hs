module MinCaml.Beta
  ( f
  ) where

import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

find :: Id.T -> Map.Map Id.T Id.T -> Id.T
find x env = fromMaybe x $ Map.lookup x env

g :: Map.Map Id.T Id.T -> KNormal.T -> KNormal.T
g env KNormal.Unit = KNormal.Unit
g env (KNormal.Int i) = KNormal.Int i
g env (KNormal.Neg x) = KNormal.Neg $ find x env
g env (KNormal.Add x y) = KNormal.Add (find x env) $ find y env
g env (KNormal.Sub x y) = KNormal.Sub (find x env) $ find y env
g env (KNormal.IfEq x y e1 e2) = KNormal.IfEq (find x env) (find y env) (g env e1) $ g env e2
g env (KNormal.IfLe x y e1 e2) = KNormal.IfLe (find x env) (find y env) (g env e1) $ g env e2
g env (KNormal.Let (x, t) e1 e2) =
  case g env e1 of
    KNormal.Var y -> g (Map.insert x y env) e2
    e1' ->
      let e2' = g env e2
      in KNormal.Let (x, t) e1' e2'
g env (KNormal.LetRec (KNormal.Fundef xt yts e1) e2) = KNormal.LetRec (KNormal.Fundef xt yts $ g env e1) $ g env e2
g env (KNormal.Var x) = KNormal.Var $ find x env
g env (KNormal.App x xs) = KNormal.App (find x env) $ fmap (`find` env) xs

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g Map.empty e
