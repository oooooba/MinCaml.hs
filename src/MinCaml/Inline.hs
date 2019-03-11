module MinCaml.Inline
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal
import qualified MinCaml.Type    as Type

size :: KNormal.T -> Int
size (KNormal.IfEq _ _ e1 e2)                    = 1 + size e1 + size e2
size (KNormal.IfLe _ _ e1 e2)                    = 1 + size e1 + size e2
size (KNormal.Let _ e1 e2)                       = 1 + size e1 + size e2
size (KNormal.LetRec (KNormal.Fundef _ _ e1) e2) = 1 + size e1 + size e2
size _                                           = 1

g :: Int -> Map.Map Id.T ([(Id.T, Type.Type)], KNormal.T) -> KNormal.T -> KNormal.T
g threshold env (KNormal.IfEq x y e1 e2) = KNormal.IfEq x y (g threshold env e1) $ g threshold env e2
g threshold env (KNormal.IfLe x y e1 e2) = KNormal.IfLe x y (g threshold env e1) $ g threshold env e2
g threshold env (KNormal.Let xt e1 e2) = KNormal.Let xt (g threshold env e1) $ g threshold env e2
g threshold env (KNormal.LetRec (KNormal.Fundef (x, t) yts e1) e2) =
  let env' =
        if size e1 > threshold
          then env
          else Map.insert x (yts, e1) env
  in KNormal.LetRec (KNormal.Fundef (x, t) yts $ g threshold env' e1) $ g threshold env' e2
g threshold env e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = do
  let threshold = 10000 -- ToDo: fix to read from MinCaml monad
  return $ g threshold Map.empty e
