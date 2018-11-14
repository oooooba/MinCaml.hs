module MinCaml.Asm where

import qualified MinCaml.Id   as Id
import qualified MinCaml.Type as Type

data T
  = Ans Exp
  | Let (Id.T, Type.Type)
        Exp
        T
  deriving (Show, Eq)

data Exp
  = Nop
  | Set Int
  deriving (Show, Eq)

data Fundef = Fundef
  { name  :: Id.L
  , args  :: [Id.L]
  , fargs :: [Id.T]
  , body  :: T
  , ret   :: Type.Type
  } deriving (Show, Eq)

data Prog =
  Prog [(Id.L, Float)]
       [Fundef]
       T
  deriving (Show, Eq)
