module MinCaml.Closure where

import           Control.Monad              (liftM2)
import           Control.Monad.State.Strict (State, get, modify, runState)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.KNormal            as KNormal
import qualified MinCaml.Type               as Type

data T
  = Unit
  | Int Int
  | Neg Id.T
  | Add Id.T
        Id.T
  | Sub Id.T
        Id.T
  | IfEq Id.T
         Id.T
         T
         T
  | IfLe Id.T
         Id.T
         T
         T
  | Let (Id.T, Type.Type)
        T
        T
  | Var Id.T
  deriving (Show, Eq)

data Fundef = Fundef
  { name     :: (Id.L, Type.Type)
  , args     :: [(Id.T, Type.Type)]
  , formalFv :: [(Id.T, Type.Type)]
  , body     :: T
  } deriving (Show, Eq)

data Prog =
  Prog [Fundef]
       T
  deriving (Show, Eq)

data ClosureStatus = ClosureStatus
  { globalStatus :: GlobalStatus
  , toplevel     :: [Fundef]
  }

type MinCamlClosure a = State ClosureStatus a

g :: Map.Map Id.T Type.Type -> Set.Set Id.T -> KNormal.T -> MinCamlClosure T
g _ _ KNormal.Unit = return Unit
g _ _ (KNormal.Int i) = return $ Int i
g _ _ (KNormal.Neg x) = return $ Neg x
g _ _ (KNormal.Add x y) = return $ Add x y
g _ _ (KNormal.Sub x y) = return $ Sub x y
g env known (KNormal.IfEq x y e1 e2) = liftM2 (IfEq x y) (g env known e1) (g env known e2)
g env known (KNormal.IfLe x y e1 e2) = liftM2 (IfLe x y) (g env known e1) (g env known e2)
g env known (KNormal.Let (x, t) e1 e2) = liftM2 (Let (x, t)) (g env known e1) (g env known e2)
g _ _ (KNormal.Var x) = return $ Var x

f :: KNormal.T -> MinCaml Prog
f e = do
  gs <- get
  let (e', cs) = runState (g Map.empty Set.empty e) ClosureStatus {globalStatus = gs, toplevel = []}
  return $ Prog (reverse $ toplevel cs) e'
