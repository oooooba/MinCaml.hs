module MinCaml.Global where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.Map                   as Map

import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data GlobalStatus = GlobalStatus
  { extenv           :: Map.Map Id.T Type.Type
  , tyVarIdCounter   :: Type.TypeVarId
  , tyVarIdToTypeEnv :: Map.Map Type.TypeVarId Type.Type
  }

type MinCaml a = ExceptT String (StateT GlobalStatus Identity) a

genType :: MinCaml Type.Type
genType = do
  varId <- fmap tyVarIdCounter get
  modify (\s -> s {tyVarIdCounter = varId + 1})
  return $ Type.Var varId

initialGlobalStatus :: GlobalStatus
initialGlobalStatus = GlobalStatus {extenv = Map.empty, tyVarIdCounter = 0, tyVarIdToTypeEnv = Map.empty}

runMinCaml :: MinCaml a -> GlobalStatus -> (Either String a, GlobalStatus)
runMinCaml e s = runIdentity (runStateT (runExceptT e) s)

evalMinCaml :: MinCaml a -> GlobalStatus -> Either String a
evalMinCaml e s = fst (runMinCaml e s)
