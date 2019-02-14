module MinCaml.Syntax where

import qualified MinCaml.Id   as Id
import qualified MinCaml.Type as Type

data T
  = Unit
  | Bool Bool
  | Int Int
  | Not T
  | Neg T
  | Add T
        T
  | Sub T
        T
  | Eq T
       T
  | Le T
       T
  | If T
       T
       T
  | Let (Id.T, Type.Type)
        T
        T
  | Var Id.T
  | LetRec Fundef
           T
  | App T
        [T]
  | Tuple [T]
  | LetTuple [(Id.T, Type.Type)]
             T
             T
  | Array T
          T
  | Get T
        T
  | Put T
        T
        T
  deriving (Show, Eq)

data Fundef = Fundef
  { name :: (Id.T, Type.Type)
  , args :: [(Id.T, Type.Type)]
  , body :: T
  } deriving (Show, Eq)
