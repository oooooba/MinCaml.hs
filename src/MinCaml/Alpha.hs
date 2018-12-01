module MinCaml.Alpha
  ( f
  ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (liftM2)
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.KNormal     as KNormal

find :: Id.T -> Map.Map Id.T Id.T -> Id.T
find x = Map.findWithDefault x x

g :: Map.Map Id.T Id.T -> KNormal.T -> MinCaml KNormal.T
g _ KNormal.Unit = return KNormal.Unit
g _ (KNormal.Int i) = return $ KNormal.Int i
g env (KNormal.Neg x) = return $ KNormal.Neg $ find x env
g env (KNormal.Add x y) = return $ KNormal.Add (find x env) (find y env)
g env (KNormal.Sub x y) = return $ KNormal.Sub (find x env) (find y env)
g env (KNormal.IfEq x y e1 e2) = liftM2 (KNormal.IfEq (find x env) (find y env)) (g env e1) (g env e2)
g env (KNormal.IfLe x y e1 e2) = liftM2 (KNormal.IfLe (find x env) (find y env)) (g env e1) (g env e2)
g env (KNormal.Let (x, t) e1 e2) = do
  x' <- genId x
  liftM2 (KNormal.Let (x', t)) (g env e1) $ g (Map.insert x x' env) e2
g env (KNormal.Var x) = return $ KNormal.Var $ find x env
g env (KNormal.LetRec (KNormal.Fundef (x, t) yts e1) e2) = do
  x' <- genId x
  let env' = Map.insert x x' env
  let ys = fmap fst yts
  ys' <- mapM genId ys
  let env'' = foldl (\e (v1, v2) -> Map.insert v1 v2 e) env' $ zip ys ys'
  fundef <- KNormal.Fundef (find x env', t) (fmap (\(y, t) -> (find y env'', t)) yts) <$> g env'' e1
  KNormal.LetRec fundef <$> g env' e2
g env (KNormal.App x ys) = return $ KNormal.App (find x env) $ fmap (`find` env) ys

f :: KNormal.T -> MinCaml KNormal.T
f = g Map.empty
