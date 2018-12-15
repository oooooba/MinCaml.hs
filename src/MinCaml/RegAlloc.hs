module MinCaml.RegAlloc
  ( f
  ) where

import           Data.Either    (fromRight)
import qualified Data.Map       as Map

import qualified MinCaml.Asm    as Asm
import           MinCaml.Global
import qualified MinCaml.Id     as Id
import qualified MinCaml.Type   as Type

data Exc
  = NotFound
  | NoReg Id.T
          Type.Type
  deriving (Show, Eq)

type MinCamlRegAlloc a = Either Exc a

type RegEnv = Map.Map Id.T Id.T

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
