module MinCaml.Simm
  ( f
  ) where

import qualified Data.Map       as Map

import qualified MinCaml.Asm    as Asm
import           MinCaml.Global
import qualified MinCaml.Id     as Id

g :: Map.Map Id.T Int -> Asm.T -> Asm.T
g env (Asm.Ans exp) = Asm.Ans $ g' env exp

g' :: Map.Map Id.T Int -> Asm.Exp -> Asm.Exp
g' _ e = e

h :: Asm.Fundef -> Asm.Fundef
h fundef@(Asm.Fundef _ _ _ e _) = fundef {Asm.body = g Map.empty e}

f :: Asm.Prog -> MinCaml Asm.Prog
f (Asm.Prog fdata fundefs e) = return $ Asm.Prog fdata (fmap h fundefs) (g Map.empty e)
