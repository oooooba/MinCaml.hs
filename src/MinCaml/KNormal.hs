module MinCaml.KNormal
  ( T(..)
  , Fundef(..)
  , f
  , f2
  ) where

import           Control.Applicative ((<$>))
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.Syntax      as Syntax
import qualified MinCaml.Type        as Type
import qualified MinCaml.Util        as Util

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
  | App Id.T
        [Id.T]
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
  (e1', _) <- g (Util.addList yts env') e1
  return (LetRec (Fundef (x, t) yts e1') e2', t2)
g env (Syntax.App e1 e2s) = do
  p1 <- g env e1
  case p1 of
    (_, Type.Fun _ t) ->
      insertLet p1 $ \f ->
        let bind xs [] = return (App f xs, t)
            bind xs (e2:e2s) = do
              p2 <- g env e2
              insertLet p2 (\x -> bind (xs ++ [x]) e2s)
        in bind [] e2s
    _ -> error "assert"

f :: Syntax.T -> MinCaml T
f e = fst <$> g Map.empty e

insertLet2 :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml (Id.T, Type.Type, T -> T)
insertLet2 env (Syntax.Var x) = return (x, env Map.! x, id)
insertLet2 env e = do
  (e', t) <- g2 env e
  x <- genVar t
  return (x, t, Let (x, t) e')

g2BinOpHelper ::
     Map.Map Id.T Type.Type -> Syntax.T -> Syntax.T -> (Id.T -> Id.T -> T) -> Type.Type -> MinCaml (T, Type.Type)
g2BinOpHelper env e1 e2 c t = do
  (x1, _, k1) <- insertLet2 env e1
  (x2, _, k2) <- insertLet2 env e2
  return (k1 $ k2 $ c x1 x2, t)

g2IfCmpHelper ::
     Map.Map Id.T Type.Type
  -> Syntax.T
  -> Syntax.T
  -> Syntax.T
  -> Syntax.T
  -> (Id.T -> Id.T -> T -> T -> T)
  -> MinCaml (T, Type.Type)
g2IfCmpHelper env e1 e2 et ef c = do
  (x1, _, k1) <- insertLet2 env e1
  (x2, _, k2) <- insertLet2 env e2
  (et', t) <- g2 env et
  (ef', _) <- g2 env ef
  return (k1 $ k2 $ c x1 x2 et' ef', t)

g2 :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml (T, Type.Type)
g2 _ Syntax.Unit = return (Unit, Type.Unit)
g2 _ (Syntax.Bool b) = return (Int $ fromEnum b, Type.Int)
g2 _ (Syntax.Int i) = return (Int i, Type.Int)
g2 env (Syntax.Not e) = g2 env $ Syntax.If e (Syntax.Bool False) (Syntax.Bool True)
g2 env (Syntax.Neg e) = do
  (x, _, k) <- insertLet2 env e
  return (k $ Neg x, Type.Int)
g2 env (Syntax.Add e1 e2) = g2BinOpHelper env e1 e2 Add Type.Int
g2 env (Syntax.Sub e1 e2) = g2BinOpHelper env e1 e2 Sub Type.Int
g2 env cmp@(Syntax.Eq _ _) = g2 env $ Syntax.If cmp (Syntax.Bool True) (Syntax.Bool False)
g2 env cmp@(Syntax.Le _ _) = g2 env $ Syntax.If cmp (Syntax.Bool True) (Syntax.Bool False)
g2 env (Syntax.If (Syntax.Not e1) e2 e3) = g2 env $ Syntax.If e1 e3 e2
g2 env (Syntax.If (Syntax.Eq e1 e2) e3 e4) = g2IfCmpHelper env e1 e2 e3 e4 IfEq
g2 env (Syntax.If (Syntax.Le e1 e2) e3 e4) = g2IfCmpHelper env e1 e2 e3 e4 IfLe
g2 env (Syntax.If e1 e2 e3) = g env $ Syntax.If (Syntax.Eq e1 $ Syntax.Bool False) e3 e2
g2 env (Syntax.Let (x, t) e1 e2) = do
  (e1', _) <- g2 env e1
  (e2', t2) <- g2 (Map.insert x t env) e2
  return (Let (x, t) e1' e2', t2)
g2 env (Syntax.Var x) = return (Var x, env Map.! x)
g2 env (Syntax.LetRec (Syntax.Fundef (x, t) yts e1) e2) = do
  let env' = Map.insert x t env
  (e2', t2) <- g2 env' e2
  (e1', _) <- g2 (Util.addList yts env') e1
  return (LetRec (Fundef (x, t) yts e1') e2', t2)
g2 env (Syntax.App e1 e2s) = do
  (x1, t1, k1) <- insertLet2 env e1
  case t1 of
    Type.Fun _ t -> do
      let bind (x2s, k2s) [] = return (k1 $ foldl (\e k -> k e) (App x1 $ reverse x2s) k2s, t)
          bind (x2s, k2s) (e2:e2s) = do
            (x2, _, k2) <- insertLet2 env e2
            bind (x2 : x2s, k2 : k2s) e2s
      bind ([], []) e2s
    _ -> error "assert"

f2 :: Syntax.T -> MinCaml T
f2 e = fst <$> g2 Map.empty e
