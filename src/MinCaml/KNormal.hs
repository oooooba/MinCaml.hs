module MinCaml.KNormal
  ( T(..)
  , Fundef(..)
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
  | LetRec Fundef
           T
  deriving (Show, Eq)

data Fundef = Fundef
  { name :: (Id.T, Type.Type)
  , args :: [(Id.T, Type.Type)]
  , body :: T
  } deriving (Show, Eq)

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
  insertLet p1 $ \v1 -> do
    p2 <- g env e2
    insertLet p2 $ \v2 -> return (c v1 v2, t)

gIfCmpHelper ::
     Map.Map Id.T Type.Type
  -> Syntax.T
  -> Syntax.T
  -> Syntax.T
  -> Syntax.T
  -> (Id.T -> Id.T -> T -> T -> T)
  -> MinCaml (T, Type.Type)
gIfCmpHelper env e1 e2 et ef c = do
  p1 <- g env e1
  insertLet p1 $ \v1 -> do
    p2 <- g env e2
    insertLet p2 $ \v2 -> do
      (et', t3) <- g env et
      (ef', _) <- g env ef
      return (c v1 v2 et' ef', t3)

g :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml (T, Type.Type)
g _ Syntax.Unit = return (Unit, Type.Unit)
g _ (Syntax.Bool b) = return (Int $ fromEnum b, Type.Int)
g _ (Syntax.Int i) = return (Int i, Type.Int)
g env (Syntax.Not e) = g env $ Syntax.If e (Syntax.Bool False) (Syntax.Bool True)
g env (Syntax.Neg e) = g env e >>= flip insertLet (\x -> return (Neg x, Type.Int))
g env (Syntax.Add e1 e2) = gBinOpHelper env e1 e2 Add Type.Int
g env (Syntax.Sub e1 e2) = gBinOpHelper env e1 e2 Sub Type.Int
g env cmp@(Syntax.Eq _ _) = g env $ Syntax.If cmp (Syntax.Bool True) (Syntax.Bool False)
g env cmp@(Syntax.Le _ _) = g env $ Syntax.If cmp (Syntax.Bool True) (Syntax.Bool False)
g env (Syntax.If (Syntax.Not e1) e2 e3) = g env $ Syntax.If e1 e3 e2
g env (Syntax.If (Syntax.Eq e1 e2) e3 e4) = gIfCmpHelper env e1 e2 e3 e4 IfEq
g env (Syntax.If (Syntax.Le e1 e2) e3 e4) = gIfCmpHelper env e1 e2 e3 e4 IfLe
g env (Syntax.If e1 e2 e3) = g env $ Syntax.If (Syntax.Eq e1 $ Syntax.Bool False) e3 e2
g env (Syntax.Let (x, t) e1 e2) = do
  (e1', _) <- g env e1
  (e2', t2) <- g (Map.insert x t env) e2
  return (Let (x, t) e1' e2', t2)
g env (Syntax.Var x)
  | Map.member x env = return (Var x, env Map.! x)
g env (Syntax.LetRec (Syntax.Fundef (x, t) yts e1) e2) = do
  let env' = Map.insert x t env
  (e2', t2) <- g env' e2
  (e1', _) <- g (foldl (\e (y, t) -> Map.insert y t e) env' yts) e1
  return (LetRec (Fundef (x, t) yts e1') e2', t2)

f :: Syntax.T -> MinCaml T
f e = fst <$> g Map.empty e
