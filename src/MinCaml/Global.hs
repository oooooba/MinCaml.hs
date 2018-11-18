module MinCaml.Global where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.Map                   as Map

import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data GlobalStatus = GlobalStatus
  { extenv           :: Map.Map Id.T Type.Type
  , idCounter        :: Int
  , varIdCounter     :: Int
  , tyVarIdCounter   :: Type.TypeVarId
  , tyVarIdToTypeEnv :: Map.Map Type.TypeVarId Type.Type
  }

type MinCaml a = ExceptT String (StateT GlobalStatus Identity) a

genId :: String -> MinCaml String
genId s = do
  id <- fmap idCounter get
  modify (\s -> s {idCounter = id + 1})
  return $ s ++ "." ++ show id

genVar :: Type.Type -> MinCaml String
genVar typ = do
  varId <- fmap varIdCounter get
  modify (\s -> s {varIdCounter = varId + 1})
  return $ "T" ++ Id.idOfType typ ++ show varId

genType :: MinCaml Type.Type
genType = do
  tyVarId <- fmap tyVarIdCounter get
  modify (\s -> s {tyVarIdCounter = tyVarId + 1})
  return $ Type.Var tyVarId

initialGlobalStatus :: GlobalStatus
initialGlobalStatus =
  GlobalStatus {extenv = Map.empty, idCounter = 0, varIdCounter = 0, tyVarIdCounter = 0, tyVarIdToTypeEnv = Map.empty}

runMinCaml :: MinCaml a -> GlobalStatus -> (Either String a, GlobalStatus)
runMinCaml e s = runIdentity (runStateT (runExceptT e) s)

evalMinCaml :: MinCaml a -> GlobalStatus -> Either String a
evalMinCaml e s = fst (runMinCaml e s)
