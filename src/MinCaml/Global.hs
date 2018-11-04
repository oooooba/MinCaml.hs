module MinCaml.Global where

import           Control.Monad.State.Strict
import qualified Data.Map                   as Map

import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data GlobalStatus = GlobalStatus
  { extenv           :: Map.Map Id.T Type.Type
  , tyVarIdCounter   :: Type.TypeVarId
  , tyVarIdToTypeEnv :: Map.Map Type.TypeVarId Type.Type
  }

type MinCaml a = State GlobalStatus a

genType :: MinCaml Type.Type
genType = do
  varId <- fmap tyVarIdCounter get
  modify (\s -> s {tyVarIdCounter = varId + 1})
  return $ Type.Var varId

initialGlobalStatus :: GlobalStatus
initialGlobalStatus = GlobalStatus {extenv = Map.empty, tyVarIdCounter = 0, tyVarIdToTypeEnv = Map.empty}

runMinCaml :: MinCaml a -> GlobalStatus -> (a, GlobalStatus)
runMinCaml = runState

evalMinCaml :: MinCaml a -> GlobalStatus -> a
evalMinCaml = evalState
