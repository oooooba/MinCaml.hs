module MinCaml.Closure
  ( Closure(..)
  , Fundef(..)
  , T(..)
  , Prog(..)
  , f
  ) where

import           Control.Applicative        ((<$>))
import           Control.Monad              (liftM2)
import           Control.Monad.State.Strict (State, get, modify, runState)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.KNormal            as KNormal
import qualified MinCaml.Type               as Type
import qualified MinCaml.Util               as Util

data Closure = Closure
  { entry    :: Id.L
  , actualFv :: [Id.T]
  } deriving (Show, Eq)

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
  | MakeCls (Id.T, Type.Type)
            Closure
            T
  | AppCls Id.T
           [Id.T]
  | AppDir Id.L
           [Id.T]
  | Tuple [Id.T]
  | LetTuple [(Id.T, Type.Type)]
             Id.T
             T
  | Get Id.T
        Id.T
  | Put Id.T
        Id.T
        Id.T
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

fv :: T -> Set.Set Id.T
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLe x y e1 e2) = Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x, t) e1 e2) = Set.union (fv e1) $ Set.delete x $ fv e2
fv (Var x) = Set.singleton x
fv (MakeCls (x, t) (Closure l ys) e) = Set.delete x $ Set.union (Set.fromList ys) (fv e)
fv (AppCls x ys) = Set.fromList $ x : ys
fv (AppDir _ xs) = Set.fromList xs
fv (Get x y) = Set.fromList [x, y]
fv (Put x y z) = Set.fromList [x, y, z]

g :: Map.Map Id.T Type.Type -> Set.Set Id.T -> KNormal.T -> MinCamlClosure T
g _ _ KNormal.Unit = return Unit
g _ _ (KNormal.Int i) = return $ Int i
g _ _ (KNormal.Neg x) = return $ Neg x
g _ _ (KNormal.Add x y) = return $ Add x y
g _ _ (KNormal.Sub x y) = return $ Sub x y
g env known (KNormal.IfEq x y e1 e2) = liftM2 (IfEq x y) (g env known e1) (g env known e2)
g env known (KNormal.IfLe x y e1 e2) = liftM2 (IfLe x y) (g env known e1) (g env known e2)
g env known (KNormal.Let (x, t) e1 e2) = liftM2 (Let (x, t)) (g env known e1) (g (Map.insert x t env) known e2)
g _ _ (KNormal.Var x) = return $ Var x
g env known (KNormal.LetRec (KNormal.Fundef (x, t) yts e1) e2) = do
  toplevelBackup <- fmap toplevel get
  let env' = Map.insert x t env
  let known' = Set.insert x known
  e1' <- g (Util.addList yts env') known' e1
  let zs = Set.difference (fv e1') (Set.fromList $ fmap fst yts)
  (known'', e1'') <-
    if Set.null zs
      then return (known', e1')
      else do
        modify (\s -> s {toplevel = toplevelBackup})
        e1' <- g (Util.addList yts env') known e1
        return (known, e1')
  let zs = Set.toList $ Set.difference (fv e1'') (Set.insert x $ Set.fromList $ fmap fst yts)
  let zts = fmap (\z -> (z, env' Map.! z)) zs
  currentToplevel <- fmap toplevel get
  let fundef = Fundef (Id.L x, t) yts zts e1''
  modify (\s -> s {toplevel = fundef : currentToplevel})
  e2' <- g env' known'' e2
  return $
    if x `elem` fv e2'
      then MakeCls (x, t) (Closure (Id.L x) zs) e2'
      else e2'
g env known (KNormal.App x ys)
  | x `elem` known = return $ AppDir (Id.L x) ys
g env known (KNormal.App f xs) = return $ AppCls f xs
g env known (KNormal.Tuple xs) = return $ Tuple xs
g env known (KNormal.LetTuple xts y e) = LetTuple xts y <$> g (Util.addList xts env) known e
g env known (KNormal.Get x y) = return $ Get x y
g env known (KNormal.Put x y z) = return $ Put x y z
g env known (KNormal.ExtFunApp x ys) = return $ AppDir (Id.L $ "min_caml_" ++ x) ys

f :: KNormal.T -> MinCaml Prog
f e = do
  gs <- get
  let (e', cs) = runState (g Map.empty Set.empty e) ClosureStatus {globalStatus = gs, toplevel = []}
  return $ Prog (reverse $ toplevel cs) e'
