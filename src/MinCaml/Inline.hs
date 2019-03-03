module MinCaml.Inline
  ( f
  ) where

import qualified Data.Map        as Map

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

size :: KNormal.T -> Int
size (KNormal.Let _ e1 e2) = 1 + size e1 + size e2
size _                     = 1

g :: Int -> Map.Map Id.T KNormal.T -> KNormal.T -> KNormal.T
g threshold env (KNormal.Let xt e1 e2) = KNormal.Let xt (g threshold env e1) $ g threshold env e2
g threshold env e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = do
  let threshold = 0 -- ToDo: fix to read from MinCaml monad
  return $ g threshold Map.empty e
