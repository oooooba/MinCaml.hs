module MinCaml.Asm where

import qualified MinCaml.Id   as Id
import qualified MinCaml.Type as Type

data IdOrImm
  = V Id.T
  | C Int
  deriving (Show, Eq)

data T
  = Ans Exp
  | Let (Id.T, Type.Type)
        Exp
        T
  deriving (Show, Eq)

data Exp
  = Nop
  | Set Int
  | Add Id.T
        IdOrImm
  | Sub Id.T
        IdOrImm
  | IfEq Id.T
         IdOrImm
         T
         T
  | IfLe Id.T
         IdOrImm
         T
         T
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

concat :: T -> (Id.T, Type.Type) -> T -> T
concat (Ans exp) xt e        = Let xt exp e
concat (Let xt exp e1) yt e2 = Let xt exp $ MinCaml.Asm.concat e1 yt e2
