module MinCaml.Closure where

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
g _ _ KNormal.Unit    = return Unit
g _ _ (KNormal.Int i) = return $ Int i

f :: KNormal.T -> MinCaml Prog
f e = do
  gs <- get
  let (e', cs) = runState (g Map.empty Set.empty e) ClosureStatus {globalStatus = gs, toplevel = []}
  return $ Prog (reverse $ toplevel cs) e'
