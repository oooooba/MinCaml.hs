module MinCaml.Asm where

import qualified Data.Set     as Set

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

removeAndUniq :: Ord a => Set.Set a -> [a] -> [a]
removeAndUniq _ [] = []
removeAndUniq xs (x:ys)
  | x `Set.member` xs = removeAndUniq xs ys
removeAndUniq xs (x:ys) = x : removeAndUniq (Set.insert x xs) ys

fvIdOrImm :: IdOrImm -> [Id.T]
fvIdOrImm (V x) = [x]
fvIdOrImm _     = []

fvExp :: Exp -> [Id.T]
fvExp (Set _) = []
fvExp (Add x y') = x : fvIdOrImm y'
fvExp (Sub x y') = x : fvIdOrImm y'
fvExp (IfEq x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fvHelper e1 ++ fvHelper e2)
fvExp (IfLe x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fvHelper e1 ++ fvHelper e2)

fvHelper :: T -> [Id.T]
fvHelper (Ans exp) = fvExp exp
fvHelper (Let (x, _) exp e) = fvExp exp ++ removeAndUniq (Set.singleton x) (fvHelper e)

fv :: T -> [Id.T]
fv e = removeAndUniq Set.empty $ fvHelper e

concat :: T -> (Id.T, Type.Type) -> T -> T
concat (Ans exp) xt e        = Let xt exp e
concat (Let xt exp e1) yt e2 = Let xt exp $ MinCaml.Asm.concat e1 yt e2
