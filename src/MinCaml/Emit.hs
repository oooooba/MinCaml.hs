module MinCaml.Emit
  ( f
  ) where

import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)

import qualified MinCaml.Asm                as Asm
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data EmitStatus = EmitStatus
  { globalStatus :: GlobalStatus
  , buffer       :: [[String]]
  } deriving (Show, Eq)

type MinCamlEmit a = StateT EmitStatus Identity a

out :: [String] -> MinCamlEmit ()
out line = do
  s <- get
  let buf = buffer s
  put $ s {buffer = line : buf}

h :: Asm.Fundef -> MinCamlEmit ()
h = undefined

run :: MinCamlEmit a -> EmitStatus -> (a, EmitStatus)
run e s = runIdentity $ runStateT e s

fAux :: Asm.Prog -> MinCamlEmit ()
fAux (Asm.Prog fdata fundefs e) = do
  out [".data"]
  out [".balign", show 8]
  mapM_ (\(Id.L x, d) -> out [show x, show d]) fdata
  out [".text"]
  mapM_ h fundefs
  out [".global", "min_caml_start"]
  out ["min_caml_start:"]

f :: Asm.Prog -> MinCaml [[String]]
f prog = do
  gs <- get
  let es = EmitStatus gs []
      (_, es') = run (fAux prog) es
      gs' = globalStatus es'
  put gs'
  return . reverse $ buffer es'
