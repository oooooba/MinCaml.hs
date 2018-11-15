module MinCaml.Virtual where

import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, runStateT)
import qualified Data.Map                   as Map

import qualified MinCaml.Asm                as Asm
import qualified MinCaml.Closure            as Closure
import           MinCaml.Global
import qualified MinCaml.Id                 as Id

data VirtualStatus = VirtualStatus
  { globalStatus :: GlobalStatus
  , fdata        :: [(Id.L, Float)]
  }

type MinCamlVirtual a = ExceptT String (StateT VirtualStatus Identity) a

g :: Map.Map Id.T Closure.T -> Closure.T -> MinCamlVirtual Asm.T
g _ Closure.Unit    = return $ Asm.Ans Asm.Nop
g _ (Closure.Int i) = return $ Asm.Ans (Asm.Set i)

h :: Closure.Fundef -> Asm.Fundef
h = undefined

f :: Closure.Prog -> MinCaml Asm.Prog
f (Closure.Prog fundefs e) = do
  gs <- get
  let fundefs' = fmap h fundefs
  let (e', vs) = runIdentity (runStateT (runExceptT $ g Map.empty e) VirtualStatus {globalStatus = gs, fdata = []})
  case e' of
    Left msg  -> throwError msg
    Right e'' -> return $ Asm.Prog (fdata vs) fundefs' e''
