module MinCaml.Simm
  ( f
  ) where

import qualified Data.Map       as Map

import qualified MinCaml.Asm    as Asm
import           MinCaml.Global
import qualified MinCaml.Id     as Id

g :: Map.Map Id.T Int -> Asm.T -> Asm.T
g env (Asm.Ans exp) = Asm.Ans $ g' env exp
g env (Asm.Let (x, t) (Asm.Set i) e) =
  let e' = g (Map.insert x i env) e
  in if x `elem` Asm.fv e'
       then Asm.Let (x, t) (Asm.Set i) e'
       else e'
g env (Asm.Let xt exp e) = Asm.Let xt (g' env exp) (g env e)

g' :: Map.Map Id.T Int -> Asm.Exp -> Asm.Exp
g' env (Asm.Add x (Asm.V y))
  | Map.member y env = Asm.Add x $ Asm.C $ env Map.! y
g' env (Asm.Add x (Asm.V y))
  | Map.member x env = Asm.Add y $ Asm.C $ env Map.! x
g' env (Asm.Sub x (Asm.V y))
  | Map.member y env = Asm.Sub x $ Asm.C $ env Map.! y
g' env (Asm.Ld x (Asm.V y) 1)
  | Map.member y env = Asm.Ld x (Asm.C $ env Map.! y) 1
g' env (Asm.St x y (Asm.V z) 1)
  | Map.member z env = Asm.St x y (Asm.C $ env Map.! z) 1
g' env (Asm.IfEq x (Asm.V y) e1 e2)
  | Map.member y env = Asm.IfEq x (Asm.C $ env Map.! y) (g env e1) (g env e2)
g' env (Asm.IfLe x (Asm.V y) e1 e2)
  | Map.member y env = Asm.IfLe x (Asm.C $ env Map.! y) (g env e1) (g env e2)
g' env (Asm.IfEq x y' e1 e2) = Asm.IfEq x y' (g env e1) (g env e2)
g' env (Asm.IfLe x y' e1 e2) = Asm.IfLe x y' (g env e1) (g env e2)
g' _ e = e

h :: Asm.Fundef -> Asm.Fundef
h fundef@(Asm.Fundef _ _ _ e _) = fundef {Asm.body = g Map.empty e}

f :: Asm.Prog -> MinCaml Asm.Prog
f (Asm.Prog fdata fundefs e) = return $ Asm.Prog fdata (fmap h fundefs) (g Map.empty e)
