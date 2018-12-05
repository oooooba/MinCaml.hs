module MinCaml.Virtual
  ( f
  ) where

import           Control.Applicative  ((<$>))
import           Control.Monad        (foldM)
import           Control.Monad.Except (throwError)
import qualified Data.Map             as Map

import qualified MinCaml.Asm          as Asm
import qualified MinCaml.Closure      as Closure
import           MinCaml.Global
import qualified MinCaml.Id           as Id
import qualified MinCaml.Type         as Type

classify ::
     [(Id.T, Type.Type)]
  -> (a, b)
  -> ((a, b) -> Id.T -> MinCaml (a, b))
  -> ((a, b) -> Id.T -> Type.Type -> MinCaml (a, b))
  -> MinCaml (a, b)
classify xts ini addf addi =
  foldM
    (\acc (x, t) ->
       case t of
         Type.Unit -> return acc
         _         -> addi acc x t)
    ini
    xts

separate :: [(Id.T, Type.Type)] -> MinCaml ([Id.T], [Id.T])
separate xts =
  classify
    xts
    ([], [])
    (\(int, float) x -> return (int, float ++ [x]))
    (\(int, float) x _ -> return (int ++ [x], float))

expand ::
     [(Id.T, Type.Type)]
  -> (Int, Asm.T)
  -> (Id.T -> Int -> Asm.T -> MinCaml Asm.T)
  -> (Id.T -> Type.Type -> Int -> Asm.T -> MinCaml Asm.T)
  -> MinCaml (Int, Asm.T)
expand xts ini addf addi =
  classify
    xts
    ini
    (\(offset, acc) x -> do
       let offset' = Asm.align offset
       addf x offset' acc >>= \a -> return (offset' + 8, a))
    (\(offset, acc) x t -> addi x t offset acc >>= \a -> return (offset + 4, a))

gIfHelper ::
     (Id.T -> Asm.IdOrImm -> Asm.T -> Asm.T -> Asm.Exp)
  -> Map.Map Id.T Type.Type
  -> Id.T
  -> Id.T
  -> Closure.T
  -> Closure.T
  -> MinCaml Asm.T
gIfHelper c env x y e1 e2 = do
  e1' <- g env e1
  e2' <- g env e2
  return $ Asm.Ans $ c x (Asm.V y) e1' e2'

g :: Map.Map Id.T Type.Type -> Closure.T -> MinCaml Asm.T
g _ Closure.Unit = return $ Asm.Ans Asm.Nop
g _ (Closure.Int i) = return $ Asm.Ans (Asm.Set i)
g _ (Closure.Neg x) = return $ Asm.Ans (Asm.Neg x)
g _ (Closure.Add x y) = return $ Asm.Ans (Asm.Add x $ Asm.V y)
g _ (Closure.Sub x y) = return $ Asm.Ans (Asm.Sub x $ Asm.V y)
g env (Closure.IfEq x y e1 e2) =
  case Map.lookup x env of
    Just Type.Bool -> gIfHelper Asm.IfEq env x y e1 e2
    Just Type.Int -> gIfHelper Asm.IfEq env x y e1 e2
    _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.IfLe x y e1 e2) =
  case Map.lookup x env of
    Just Type.Bool -> gIfHelper Asm.IfLe env x y e1 e2
    Just Type.Int -> gIfHelper Asm.IfLe env x y e1 e2
    _ -> throwError "equality supported only for bool, int, and float"
g env (Closure.Let (x, t) e1 e2) = do
  e1' <- g env e1
  e2' <- g (Map.insert x t env) e2
  return $ Asm.concat e1' (x, t) e2'
g env (Closure.Var x) =
  return $
  case Map.lookup x env of
    Just Type.Unit -> Asm.Ans Asm.Nop
    _              -> Asm.Ans $ Asm.Mov x
g env (Closure.MakeCls (x, t) (Closure.Closure l ys) e2) = do
  e2' <- g (Map.insert x t env) e2
  (offset, storeFv) <-
    expand
      (fmap (\y -> (y, env Map.! y)) ys)
      (4, e2')
      undefined
      (\y _ offset storeFv -> Asm.seq (Asm.St y x (Asm.C offset) 1, storeFv))
  z <- genId "l"
  cont <- Asm.seq (Asm.St z x (Asm.C 0) 1, storeFv)
  return $
    Asm.Let (x, t) (Asm.Mov Asm.regHp) $
    Asm.Let (Asm.regHp, Type.Int) (Asm.Add Asm.regHp (Asm.C $ Asm.align offset)) $
    Asm.Let (z, Type.Int) (Asm.SetL l) cont

h :: Closure.Fundef -> Asm.Fundef
h = undefined

f :: Closure.Prog -> MinCaml Asm.Prog
f (Closure.Prog fundefs e) = do
  let fundefs' = fmap h fundefs
  Asm.Prog [] fundefs' <$> g Map.empty e
