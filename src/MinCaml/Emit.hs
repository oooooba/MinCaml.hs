module MinCaml.Emit
  ( f
  , f'
  ) where

import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, modify, put,
                                             runStateT)
import           Data.Either                (fromRight)
import qualified Data.Set                   as Set

import qualified MinCaml.Asm                as Asm
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data EmitStatus = EmitStatus
  { globalStatus :: GlobalStatus
  , buffer       :: [[String]]
  , stackset     :: Set.Set Id.T
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

callMinCaml :: MinCaml a -> MinCamlEmit a
callMinCaml mc = do
  es <- get
  let gs = globalStatus es
  let (result, gs') = runMinCaml mc gs
  let es' = es {globalStatus = gs'}
  put es'
  return $ fromRight undefined result

genIdHelper :: String -> MinCamlEmit String
genIdHelper s = callMinCaml (genId s)

genVarHelper :: Type.Type -> MinCamlEmit Id.T
genVarHelper t = callMinCaml (genVar t)

ppIdOrImm :: Asm.IdOrImm -> String
ppIdOrImm (Asm.V x) = x
ppIdOrImm (Asm.C i) = "$" ++ show i

gAuxNonRetHelper :: Asm.Exp -> MinCamlEmit ()
gAuxNonRetHelper exp = do
  genVarHelper Type.Unit >>= (\x -> gAux (NonTail x, exp))
  out0 "ret"

gAuxTailIf :: Asm.T -> Asm.T -> String -> String -> MinCamlEmit ()
gAuxTailIf e1 e2 b bn = do
  bElse <- genIdHelper (b ++ "_else")
  out1 bn bElse
  stacksetBack <- fmap stackset get
  g (Tail, e1)
  out0 $ bElse ++ ":"
  modify (\s -> s {stackset = stacksetBack})
  g (Tail, e2)

gAuxNonTailIf :: Dest -> Asm.T -> Asm.T -> String -> String -> MinCamlEmit ()
gAuxNonTailIf dest e1 e2 b bn = do
  bElse <- genIdHelper (b ++ "_else")
  bCont <- genIdHelper (b ++ "_cont")
  out1 bn bElse
  stacksetBack <- fmap stackset get
  g (dest, e1)
  stackset1 <- fmap stackset get
  out1 "jmp" bCont
  out0 $ bElse ++ ":"
  modify (\s -> s {stackset = stacksetBack})
  g (dest, e2)
  out0 $ bCont ++ ":"
  stackset2 <- fmap stackset get
  modify (\s -> s {stackset = Set.intersection stackset1 stackset2})

gAux :: (Dest, Asm.Exp) -> MinCamlEmit ()
gAux (NonTail _, Asm.Nop) = return ()
gAux (NonTail x, Asm.Set i) = out2 "movl" (show i) x
gAux (NonTail x, Asm.Mov y)
  | x /= y = out2 "movl" y x
gAux (NonTail x, Asm.Mov y) = return ()
gAux (NonTail x, Asm.Neg y)
  | x /= y = out2 "movl" y x >> out1 "negl" x
gAux (NonTail x, Asm.Neg y) = out1 "negl" x
gAux (NonTail x, Asm.Add y z')
  | Asm.V x == z' = out2 "addl" y x
gAux (NonTail x, Asm.Add y z')
  | x /= y = out2 "movl" y x >> out2 "addl" (ppIdOrImm z') x
gAux (NonTail x, Asm.Add y z') = out2 "addl" (ppIdOrImm z') x
gAux (NonTail x, Asm.Sub y z')
  | Asm.V x == z' = out2 "subl" y x
gAux (NonTail x, Asm.Sub y z')
  | x /= y = out2 "movl" y x >> out2 "subl" (ppIdOrImm z') x
gAux (NonTail x, Asm.Sub y z') = out2 "subl" (ppIdOrImm z') x
gAux (Tail, exp@Asm.Nop) = gAuxNonRetHelper exp >> out0 "ret"
gAux (Tail, exp@(Asm.Set _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"
gAux (Tail, exp@(Asm.Mov _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"
gAux (Tail, exp@(Asm.Neg _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"
gAux (Tail, exp@(Asm.Add _ _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"
gAux (Tail, exp@(Asm.Sub _ _)) = gAux (NonTail $ head Asm.regs, exp) >> out0 "ret"
gAux (Tail, Asm.IfEq x y' e1 e2) = out2 "cmpl" (ppIdOrImm y') x >> gAuxTailIf e1 e2 "je" "jne"
gAux (Tail, Asm.IfLe x y' e1 e2) = out2 "cmpl" (ppIdOrImm y') x >> gAuxTailIf e1 e2 "jle" "jg"
gAux (NonTail z, Asm.IfEq x y' e1 e2) = out2 "cmpl" (ppIdOrImm y') x >> gAuxNonTailIf (NonTail z) e1 e2 "je" "jne"
gAux (NonTail z, Asm.IfLe x y' e1 e2) = out2 "cmpl" (ppIdOrImm y') x >> gAuxNonTailIf (NonTail z) e1 e2 "jle" "jg"

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
  let es = EmitStatus gs [] Set.empty
      (_, es') = run (mapM_ (\(Id.L x, d) -> out [show x, show d]) fdata) es
      asmFdata = reverse $ buffer es'
      (_, es'') = run (mapM_ h fundefs) es'
      asmFundefs = reverse $ buffer es''
      (_, es''') = run (g (NonTail $ head Asm.regs, e)) es'
      asmE = reverse $ buffer es'''
      gs' = globalStatus es'''
  put gs'
  return (asmFdata, asmFundefs, asmE)

f' :: Asm.Prog -> MinCaml [[String]]
f' prog = do
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
