module MinCaml.Closure
  ( Fundef
  , T(..)
  , Prog(..)
  , f
  ) where

import           Control.Monad              (liftM2)
import           Control.Monad.State.Strict (State, get, modify, runState)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.KNormal            as KNormal
import qualified MinCaml.Type               as Type

data Closure = Closure
  { entry    :: Id.L
  , actualFv :: [Id.T]
  } deriving (Show, Eq)

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
  | MakeCls (Id.T, Type.Type)
            Closure
            T
  | AppCls Id.T
           [Id.T]
  | AppDir Id.L
           [Id.T]
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

fv :: T -> Set.Set Id.T
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLe x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x, t) e1 e2) = Set.union (fv e1) $ Set.delete x $ fv e2
fv (Var x) = Set.singleton x
fv (MakeCls (x, t) (Closure l ys) e) = Set.delete x $ Set.union (Set.fromList ys) (fv e)
fv (AppCls x ys) = Set.fromList $ x : ys
fv (AppDir _ xs) = Set.fromList xs

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
