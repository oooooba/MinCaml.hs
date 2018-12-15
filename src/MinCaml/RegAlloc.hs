module MinCaml.RegAlloc
  ( f
  ) where

import           Data.Either    (fromRight)
import qualified Data.Map       as Map
import           Prelude        hiding (return)
import qualified Prelude

import qualified MinCaml.Asm    as Asm
import           MinCaml.Global
import qualified MinCaml.Id     as Id
import qualified MinCaml.Type   as Type

data Exc
  = NotFound
  | NoReg Id.T
          Type.Type
  deriving (Show, Eq)

type MinCamlRegAlloc a = Either Exc (MinCaml a)

type RegEnv = Map.Map Id.T Id.T

return :: a -> MinCamlRegAlloc a
return = Right . Prelude.return

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

h :: Asm.Fundef -> MinCamlRegAlloc Asm.Fundef
h = undefined

f :: Asm.Prog -> MinCaml Asm.Prog
f (Asm.Prog fdata fundefs e) = do
  fundefs' <- sequence $ fromRight undefined $ mapM h fundefs
  tmp <- genVar Type.Unit
  (e', regenv') <- fromRight undefined $ g (tmp, Type.Unit) (Asm.Ans Asm.Nop) Map.empty e
  Prelude.return $ Asm.Prog fdata fundefs' e'
