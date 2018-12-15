module MinCaml.RegAlloc
  ( f
  ) where

import           Control.Exception (assert)
import           Data.Either       (fromRight)
import qualified Data.Map          as Map

import qualified MinCaml.Asm       as Asm
import           MinCaml.Global
import qualified MinCaml.Id        as Id
import qualified MinCaml.Type      as Type

data Exc
  = NotFound
  | NoReg Id.T
          Type.Type
  deriving (Show, Eq)

type MinCamlRegAlloc a = Either Exc a

type RegEnv = Map.Map Id.T Id.T

data AllocResult
  = Alloc Id.T
  | Spill Id.T
  deriving (Show, Eq)

target' :: Id.T -> (Id.T, Type.Type) -> Asm.Exp -> (Bool, [Id.T])
target' _ _ _ = (False, [])

target :: Id.T -> (Id.T, Type.Type) -> Asm.T -> (Bool, [Id.T])
target src dest (Asm.Ans exp) = target' src dest exp
target src dest (Asm.Let xt exp e) =
  let (c1, rs1) = target' src xt exp
  in if c1
       then (True, rs1)
       else let (c2, rs2) = target src dest e
            in (c2, rs1 ++ rs2)

source' :: Type.Type -> Asm.Exp -> [Id.T]
source' t (Asm.Add x (Asm.C _)) = [x]
source' t (Asm.Add x (Asm.V y)) = [x, y]
source' _ _                     = []

source :: Type.Type -> Asm.T -> [Id.T]
source t (Asm.Ans exp)   = source' t exp
source t (Asm.Let _ _ e) = source t e

alloc :: Asm.T -> RegEnv -> Id.T -> Type.Type -> [Id.T] -> MinCamlRegAlloc AllocResult
alloc = undefined

add :: Id.T -> Id.T -> RegEnv -> RegEnv
add x r regenv
  | Asm.isReg x = assert (x == r) regenv
add x r regenv = Map.insert x r regenv

find :: Id.T -> Type.Type -> RegEnv -> MinCamlRegAlloc Id.T
find x _ regenv
  | Asm.isReg x = return x
find x _ regenv
  | Map.member x regenv = return $ regenv Map.! x
find x t _ = Left $ NoReg x t

gAuxAndRestore :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.Exp -> MinCamlRegAlloc (Asm.T, RegEnv)
gAuxAndRestore dest cont regenv exp =
  case gAux dest cont regenv exp of
    Left (NoReg x t) -> g dest cont regenv $ Asm.Let (x, t) (Asm.Restore x) $ Asm.Ans exp
    result@(Right _) -> result

gAux :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.Exp -> MinCamlRegAlloc (Asm.T, RegEnv)
gAux dest cont regenv exp@Asm.Nop     = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.Set _) = return (Asm.Ans exp, regenv)

g :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.T -> MinCamlRegAlloc (Asm.T, RegEnv)
g dest cont regenv (Asm.Ans exp) = gAuxAndRestore dest cont regenv exp
g dest cont regenv (Asm.Let xt@(x, t) exp e) = undefined

-- ToDo: modify to use Global.genVar to make fresh variable
neverUsedIdentifier :: String
neverUsedIdentifier = "'never used identifier'"

h :: Asm.Fundef -> MinCamlRegAlloc Asm.Fundef
h = undefined

f :: Asm.Prog -> MinCaml Asm.Prog
f (Asm.Prog fdata fundefs e) = do
  let fundefs' = fromRight undefined $ mapM h fundefs
  let (e', _) = fromRight undefined $ g (neverUsedIdentifier ++ show 0, Type.Unit) (Asm.Ans Asm.Nop) Map.empty e
  Prelude.return $ Asm.Prog fdata fundefs' e'
