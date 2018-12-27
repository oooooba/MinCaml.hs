module MinCaml.Emit
  ( f
  ) where

import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Data.Either                (fromRight)

import qualified MinCaml.Asm                as Asm
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data EmitStatus = EmitStatus
  { globalStatus :: GlobalStatus
  , buffer       :: [[String]]
  } deriving (Show, Eq)

type MinCamlEmit a = StateT EmitStatus Identity a

data Dest
  = Tail
  | NonTail Id.T
  deriving (Show, Eq)

out :: [String] -> MinCamlEmit ()
out line = do
  s <- get
  let buf = buffer s
  put $ s {buffer = line : buf}

out0 :: String -> MinCamlEmit ()
out0 instr = out [instr]

out1 :: String -> String -> MinCamlEmit ()
out1 instr op1 = out [instr, op1]

out2 :: String -> String -> String -> MinCamlEmit ()
out2 instr op1 op2 = out [instr, op1, ",", op2]

regX86Esp :: String
regX86Esp = "%esp"

regX86Ebp :: String
regX86Ebp = "%ebp"

genVarHelper :: Type.Type -> MinCamlEmit Id.T
genVarHelper t = do
  es <- get
  let gs = globalStatus es
  let (result, gs') = runMinCaml (genVar t) gs
  let es' = es {globalStatus = gs'}
  put es'
  return $ fromRight undefined result

gAuxNonRetHelper :: Asm.Exp -> MinCamlEmit ()
gAuxNonRetHelper exp = do
  genVarHelper Type.Unit >>= (\x -> gAux (NonTail x, exp))
  out0 "ret"

gAux :: (Dest, Asm.Exp) -> MinCamlEmit ()
gAux (NonTail _, Asm.Nop) = return ()
gAux (NonTail x, Asm.Set i) = out2 "movl" (show i) x
gAux (Tail, exp@Asm.Nop) = gAuxNonRetHelper exp >> out0 "ret"
gAux (Tail, exp@(Asm.Set _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"

g :: (Dest, Asm.T) -> MinCamlEmit ()
g (dest, Asm.Ans exp)          = gAux (dest, exp)
g (dest, Asm.Let (x, t) exp e) = gAux (NonTail x, exp) >> g (dest, e)

h :: Asm.Fundef -> MinCamlEmit ()
h = undefined

run :: MinCamlEmit a -> EmitStatus -> (a, EmitStatus)
run e s = runIdentity $ runStateT e s

f :: Asm.Prog -> MinCaml ([[String]], [[String]], [[String]])
f (Asm.Prog fdata fundefs e) = do
  gs <- get
  let es = EmitStatus gs []
      (_, es') = run (mapM_ (\(Id.L x, d) -> out [show x, show d]) fdata) es
      asmFdata = reverse $ buffer es'
      (_, es'') = run (mapM_ h fundefs) es'
      asmFundefs = reverse $ buffer es''
      (_, es''') = run (g (NonTail $ head Asm.regs, e)) es'
      asmE = reverse $ buffer es'''
      gs' = globalStatus es'''
  put gs'
  return (asmFdata, asmFundefs, asmE)

fFull :: Asm.Prog -> MinCaml [[String]]
fFull prog = do
  (asmFdata, asmFundefs, asmE) <- f prog
  return $
    [[".data"], [".balign", show 8]] ++
    asmFdata ++
    [[".text"]] ++
    asmFundefs ++
    [ [".global", "min_caml_start"]
    , ["min_caml_start:"]
    , ["pushl", Asm.regEax]
    , ["pushl", Asm.regEbx]
    , ["pushl", Asm.regEcx]
    , ["pushl", Asm.regEdx]
    , ["pushl", Asm.regEsi]
    , ["pushl", Asm.regEdi]
    , ["pushl", regX86Ebp]
    , ["movl", show 32, "(", regX86Esp, ")", ",", Asm.regSp]
    , ["movl", show 36, "(", regX86Esp, ")", ",", head Asm.regs]
    , ["movl", head Asm.regs, ",", Asm.regHp]
    ] ++
    asmE ++
    [ ["popl", regX86Ebp]
    , ["popl", Asm.regEdi]
    , ["popl", Asm.regEsi]
    , ["popl", Asm.regEdx]
    , ["popl", Asm.regEcx]
    , ["popl", Asm.regEbx]
    , ["popl", Asm.regEax]
    , ["ret"]
    ]
