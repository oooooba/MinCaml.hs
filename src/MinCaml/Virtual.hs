module MinCaml.Virtual
  ( f
  ) where

import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, runStateT)
import qualified Data.Map                   as Map

import qualified MinCaml.Asm                as Asm
import qualified MinCaml.Closure            as Closure
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data VirtualStatus = VirtualStatus
  { globalStatus :: GlobalStatus
  , fdata        :: [(Id.L, Float)]
  }

type MinCamlVirtual a = ExceptT String (StateT VirtualStatus Identity) a

gIfHelper ::
     (Id.T -> Asm.IdOrImm -> Asm.T -> Asm.T -> Asm.Exp)
  -> Map.Map Id.T Type.Type
  -> Id.T
  -> Id.T
  -> Closure.T
  -> Closure.T
  -> MinCamlVirtual Asm.T
gIfHelper c env x y e1 e2 = do
  e1' <- g env e1
  e2' <- g env e2
  return $ Asm.Ans $ c x (Asm.V y) e1' e2'

g :: Map.Map Id.T Type.Type -> Closure.T -> MinCamlVirtual Asm.T
g _ Closure.Unit = return $ Asm.Ans Asm.Nop
g _ (Closure.Int i) = return $ Asm.Ans (Asm.Set i)
g _ (Closure.Neg x) = return $ Asm.Ans (Asm.Neg x)
g _ (Closure.Add x y) = return $ Asm.Ans (Asm.Add x $ Asm.V y)
g _ (Closure.Sub x y) = return $ Asm.Ans (Asm.Sub x $ Asm.V y)
g env (Closure.IfEq x y e1 e2) =
  case Map.lookup x env of
    Just Type.Bool -> gIfHelper Asm.IfEq env x y e1 e2
    Just Type.Int -> gIfHelper Asm.IfEq env x y e1 e2
    _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.IfLe x y e1 e2) =
  case Map.lookup x env of
    Just Type.Bool -> gIfHelper Asm.IfLe env x y e1 e2
    Just Type.Int -> gIfHelper Asm.IfLe env x y e1 e2
    _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.Let (x, t) e1 e2) = do
  e1' <- g env e1
  e2' <- g (Map.insert x t env) e2
  return $ Asm.concat e1' (x, t) e2'
g env (Closure.Var x) =
  return $
  case Map.lookup x env of
    Just Type.Unit -> Asm.Ans Asm.Nop
    _              -> Asm.Ans $ Asm.Mov x

h :: Closure.Fundef -> Asm.Fundef
h = undefined

f :: Closure.Prog -> MinCaml Asm.Prog
f (Closure.Prog fundefs e) = do
  gs <- get
  let fundefs' = fmap h fundefs
  let (e', vs) = runIdentity (runStateT (runExceptT $ g Map.empty e) VirtualStatus {globalStatus = gs, fdata = []})
  case e' of
    Left msg  -> throwError msg
    Right e'' -> return $ Asm.Prog (fdata vs) fundefs' e''
