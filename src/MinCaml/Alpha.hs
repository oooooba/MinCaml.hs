module MinCaml.Alpha
  ( f
  , g
  ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (liftM2)
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.KNormal     as KNormal
import qualified MinCaml.Util        as Util

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
  let env'' = Util.addList2 ys ys' env'
  fundef <- KNormal.Fundef (find x env', t) (fmap (\(y, t) -> (find y env'', t)) yts) <$> g env'' e1
  KNormal.LetRec fundef <$> g env' e2
g env (KNormal.App x ys) = return $ KNormal.App (find x env) $ fmap (`find` env) ys
g env (KNormal.Tuple xs) = return $ KNormal.Tuple $ fmap (`find` env) xs
g env (KNormal.LetTuple xts y e) = do
  let xs = fmap fst xts
  xs' <- mapM genId xs
  let env' = Util.addList2 xs xs' env
  let xts' = fmap (\(x, t) -> (find x env', t)) xts
  e' <- g env' e
  return $ KNormal.LetTuple xts' (find y env) e'
g env (KNormal.Get x y) = return $ KNormal.Get (find x env) $ find y env
g env (KNormal.Put x y z) = return $ KNormal.Put (find x env) (find y env) $ find z env
g env (KNormal.ExtFunApp x ys) = return $ KNormal.ExtFunApp x $ fmap (`find` env) ys

f :: KNormal.T -> MinCaml KNormal.T
f = g Map.empty
