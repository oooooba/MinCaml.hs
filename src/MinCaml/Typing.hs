module MinCaml.Typing
  ( f
  ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (filterM, liftM2, liftM3)
import           Control.Monad.State (get, modify, put)
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.Syntax      as Syntax
import qualified MinCaml.Type        as Type

iter2 :: Show a => (a -> a -> MinCaml ()) -> [a] -> [a] -> MinCaml ()
iter2 f xs ys =
  if length xs == length ys
    then mapM_ (uncurry f) $ zip xs ys
    else fail $ "iter2: " ++ show xs ++ ", " ++ show ys

derefType :: Type.Type -> MinCaml Type.Type
derefType (Type.Fun t1s t2) = liftM2 Type.Fun (mapM derefType t1s) (derefType t2)
derefType (Type.Tuple ts) = Type.Tuple <$> mapM derefType ts
derefType (Type.Array t) = Type.Array <$> derefType t
derefType (Type.Var tyVarId) = do
  env <- fmap tyVarIdToTypeEnv get
  case Map.lookup tyVarId env of
    Just t  -> return t
    Nothing -> return Type.Int
derefType t = return t

derefIdType :: (Id.T, Type.Type) -> MinCaml (Id.T, Type.Type)
derefIdType (x, t) = (\t -> (x, t)) <$> derefType t

derefTerm :: Syntax.T -> MinCaml Syntax.T
derefTerm Syntax.Unit = return Syntax.Unit
derefTerm (Syntax.Bool b) = return $ Syntax.Bool b
derefTerm (Syntax.Int n) = return $ Syntax.Int n
derefTerm (Syntax.Not e) = Syntax.Not <$> derefTerm e
derefTerm (Syntax.Add e1 e2) = liftM2 Syntax.Add (derefTerm e1) (derefTerm e2)
derefTerm (Syntax.Eq e1 e2) = liftM2 Syntax.Eq (derefTerm e1) (derefTerm e2)
derefTerm (Syntax.Le e1 e2) = liftM2 Syntax.Le (derefTerm e1) (derefTerm e2)
derefTerm (Syntax.If e1 e2 e3) = liftM3 Syntax.If (derefTerm e1) (derefTerm e2) (derefTerm e3)
derefTerm (Syntax.Let xt e1 e2) = liftM3 Syntax.Let (derefIdType xt) (derefTerm e1) (derefTerm e2)
derefTerm (Syntax.LetRec (Syntax.Fundef xt yts e1) e2) = do
  fundef <- liftM3 Syntax.Fundef (derefIdType xt) (mapM derefIdType yts) (derefTerm e1)
  Syntax.LetRec fundef <$> derefTerm e2
derefTerm (Syntax.App e es) = liftM2 Syntax.App (derefTerm e) (mapM derefTerm es)
derefTerm e = return e

occur :: Type.TypeVarId -> Type.Type -> MinCaml Bool
occur tyVarId1 (Type.Fun t2s t2) = do
  cond <- (not . null) <$> filterM (occur tyVarId1) t2s
  if cond
    then return True
    else occur tyVarId1 t2
occur tyVarId1 (Type.Tuple t2s) = (not . null) <$> filterM (occur tyVarId1) t2s
occur tyVarId1 (Type.Array t2) = occur tyVarId1 t2
occur tyVarId1 (Type.Var tyVarId2)
  | tyVarId1 == tyVarId2 = return True
occur tyVarId1 (Type.Var tyVarId2) = do
  res <- fmap (Map.lookup tyVarId2 . tyVarIdToTypeEnv) get
  case res of
    Just t2 -> occur tyVarId1 t2
    Nothing -> return False
occur _ _ = return False

unify :: Type.Type -> Type.Type -> MinCaml ()
unify Type.Unit Type.Unit = return ()
unify Type.Bool Type.Bool = return ()
unify Type.Int Type.Int = return ()
unify (Type.Fun t1s t1') (Type.Fun t2s t2') = iter2 unify t1s t2s >> unify t1' t2'
unify (Type.Tuple t1s) (Type.Tuple t2s) = iter2 unify t1s t2s
unify (Type.Array t1) (Type.Array t2) = unify t1 t2
unify (Type.Var r1) (Type.Var r2)
  | r1 == r2 = return ()
unify t1@(Type.Var _) t2 = unifyHelper t1 t2
unify t1 t2@(Type.Var _) = unifyHelper t2 t1
unify t1 t2 = fail $ "unify: unimplemented, " ++ show t1 ++ ", " ++ show t2

unifyHelper :: Type.Type -> Type.Type -> MinCaml ()
unifyHelper t1@(Type.Var tyVarId1) t2 = do
  res <- fmap (Map.lookup tyVarId1 . tyVarIdToTypeEnv) get
  case res of
    Just t1' -> unify t1' t2
    Nothing -> do
      cond <- occur tyVarId1 t2
      if cond
        then fail $ "unifyHelper: occur, " ++ show t1 ++ ", " ++ show t2
        else do
          e <- fmap tyVarIdToTypeEnv get
          modify (\s -> s {tyVarIdToTypeEnv = Map.insert tyVarId1 t2 e})

g :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml Type.Type
g _ Syntax.Unit = return Type.Unit
g _ (Syntax.Bool _) = return Type.Bool
g _ (Syntax.Int _) = return Type.Int
g env (Syntax.Add e1 e2) = do
  g env e1 >>= unify Type.Int
  g env e2 >>= unify Type.Int
  return Type.Int
g env (Syntax.Eq e1 e2) = gBinOpHelper env e1 e2 >> return Type.Bool
g env (Syntax.Le e1 e2) = gBinOpHelper env e1 e2 >> return Type.Bool
g env (Syntax.If e1 e2 e3) = do
  g env e1 >>= unify Type.Bool
  gBinOpHelper env e2 e3
g env (Syntax.Let (x, t) e1 e2) = do
  g env e1 >>= unify t
  g (Map.insert x t env) e2
g env (Syntax.Var x)
  | x `Map.member` env = return . fromJust $ Map.lookup x env
g _ (Syntax.Var x) = do
  env <- fmap extenv get
  case Map.lookup x env of
    Just t -> return t
    Nothing -> do
      t <- genType
      modify (\s -> s {extenv = Map.insert x t env})
      return t
g env (Syntax.LetRec (Syntax.Fundef (x, t) yts e1) e2) = do
  let env = Map.insert x t env
  e1Ty <- g (foldl (\e (y, t) -> Map.insert y t e) env yts) e1
  unify t (Type.Fun (fmap snd yts) e1Ty)
  g env e2
g env (Syntax.App e es) = do
  t <- genType
  esTy <- mapM (g env) es
  eTy <- g env e
  unify eTy $ Type.Fun esTy t
  return t

gBinOpHelper :: Map.Map Id.T Type.Type -> Syntax.T -> Syntax.T -> MinCaml Type.Type
gBinOpHelper env e1 e2 = do
  t1 <- g env e1
  t2 <- g env e2
  unify t1 t2 >> return t1

unite :: MinCaml ()
unite = do
  env <- fmap tyVarIdToTypeEnv get
  Map.traverseWithKey uniteHelper env
  return ()
  where
    uniteHelper :: Type.TypeVarId -> Type.Type -> MinCaml Type.Type
    uniteHelper tyVarId ty =
      case ty of
        Type.Var tyVarId' -> do
          ty' <- fromJust . Map.lookup tyVarId' <$> fmap tyVarIdToTypeEnv get
          ty' <- uniteHelper tyVarId' ty'
          env' <- fmap tyVarIdToTypeEnv get
          modify (\s -> s {tyVarIdToTypeEnv = Map.insert tyVarId ty' env'})
          return ty'
        _ -> return ty

f :: Syntax.T -> MinCaml Syntax.T
f e = do
  modify (\s -> s {extenv = Map.empty})
  g Map.empty e >>= unify Type.Unit
  unite
  extenv' <- fmap extenv get >>= Map.traverseWithKey (\_ t -> derefType t)
  modify (\s -> s {extenv = extenv'})
  derefTerm e
