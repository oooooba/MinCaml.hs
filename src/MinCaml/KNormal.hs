module MinCaml.KNormal
  ( T(..)
  , f
  ) where

import           Control.Applicative ((<$>))
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.Syntax      as Syntax
import qualified MinCaml.Type        as Type

data T
  = Unit
  | Int Int
  | Add Id.T
        Id.T
  | Sub Id.T
        Id.T
  | Let (Id.T, Type.Type)
        T
        T
  | Var Id.T
  deriving (Show, Eq)

insertLet :: (T, Type.Type) -> (Id.T -> MinCaml (T, Type.Type)) -> MinCaml (T, Type.Type)
insertLet (Var x, _) k = k x
insertLet (e, t) k = do
  x <- genVar t
  (e', t') <- k x
  return (Let (x, t) e e', t')

gBinOpHelper ::
     Map.Map Id.T Type.Type -> Syntax.T -> Syntax.T -> (Id.T -> Id.T -> T) -> Type.Type -> MinCaml (T, Type.Type)
gBinOpHelper env e1 e2 c t = do
  p1 <- g env e1
  insertLet p1 $ \x -> do
    p2 <- g env e2
    insertLet p2 $ \y -> return (c x y, t)

g :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml (T, Type.Type)
g _ Syntax.Unit          = return (Unit, Type.Unit)
g _ (Syntax.Int i)       = return (Int i, Type.Int)
g env (Syntax.Add e1 e2) = gBinOpHelper env e1 e2 Add Type.Int
g env (Syntax.Sub e1 e2) = gBinOpHelper env e1 e2 Sub Type.Int

f :: Syntax.T -> MinCaml T
f e = fst <$> g Map.empty e
