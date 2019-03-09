module MinCaml.Assoc
  ( f
  ) where

import           MinCaml.Global
import qualified MinCaml.Id      as Id
import qualified MinCaml.KNormal as KNormal

g :: KNormal.T -> KNormal.T
g (KNormal.IfEq x y e1 e2) = KNormal.IfEq x y (g e1) $ g e2
g (KNormal.IfLe x y e1 e2) = KNormal.IfLe x y (g e1) $ g e2
g (KNormal.Let xt e1 e2) =
  let insert (KNormal.Let yt e3 e4)     = KNormal.Let yt e3 $ insert e4
      insert (KNormal.LetRec fundefs e) = KNormal.LetRec fundefs $ insert e
      insert (KNormal.LetTuple yts z e) = KNormal.LetTuple yts z $ insert e
      insert e                          = KNormal.Let xt e $ g e2
  in insert $ g e1
g e = e

f :: KNormal.T -> MinCaml KNormal.T
f e = return $ g e
