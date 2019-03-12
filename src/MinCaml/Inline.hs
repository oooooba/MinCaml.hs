module MinCaml.Inline
  ( f
  ) where

import           Control.Monad       (liftM2)
import           Control.Monad.State (get)
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.KNormal     as KNormal
import qualified MinCaml.Type        as Type

size :: KNormal.T -> Int
size (KNormal.IfEq _ _ e1 e2)                    = 1 + size e1 + size e2
size (KNormal.IfLe _ _ e1 e2)                    = 1 + size e1 + size e2
size (KNormal.Let _ e1 e2)                       = 1 + size e1 + size e2
size (KNormal.LetRec (KNormal.Fundef _ _ e1) e2) = 1 + size e1 + size e2
size _                                           = 1

g :: Map.Map Id.T ([(Id.T, Type.Type)], KNormal.T) -> KNormal.T -> MinCaml KNormal.T
g env (KNormal.IfEq x y e1 e2) = liftM2 (KNormal.IfEq x y) (g env e1) $ g env e2
g env (KNormal.IfLe x y e1 e2) = liftM2 (KNormal.IfLe x y) (g env e1) $ g env e2
g env (KNormal.Let xt e1 e2) = liftM2 (KNormal.Let xt) (g env e1) $ g env e2
g env (KNormal.LetRec (KNormal.Fundef (x, t) yts e1) e2) = do
  threshold <- fmap inlineThreshold get
  let env' =
        if size e1 > threshold
          then env
          else Map.insert x (yts, e1) env
  e1' <- g env' e1
  e2' <- g env' e2
  return $ KNormal.LetRec (KNormal.Fundef (x, t) yts e1') e2'
g _ e = return e

f :: KNormal.T -> MinCaml KNormal.T
f = g Map.empty
