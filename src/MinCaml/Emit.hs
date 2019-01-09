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
  , buffer       :: [String]
  , stackset     :: Set.Set Id.T
  } deriving (Show, Eq)

type MinCamlEmit a = StateT EmitStatus Identity a

data Dest
  = Tail
  | NonTail Id.T
  deriving (Show, Eq)

out :: String -> MinCamlEmit ()
out line = do
  s <- get
  let buf = buffer s
  put $ s {buffer = line : buf}

regX86Rbp :: String
regX86Rbp = "%rbp"

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

ppIdOrImm :: Asm.IdOrImm -> Asm.Operand
ppIdOrImm (Asm.V x) = Asm.Reg x
ppIdOrImm (Asm.C i) = Asm.Imm i

gAuxNonRetHelper :: Asm.Exp -> MinCamlEmit ()
gAuxNonRetHelper exp = do
  genVarHelper Type.Unit >>= (\x -> gAux (NonTail x, exp))
  out Asm.instrRet

gAuxTailIf :: Asm.T -> Asm.T -> String -> (Asm.Operand -> String) -> MinCamlEmit ()
gAuxTailIf e1 e2 labelBase bn = do
  bElse <- genIdHelper (labelBase ++ "_else")
  out $ bn $ Asm.Lab bElse
  stacksetBack <- fmap stackset get
  g (Tail, e1)
  out $ Asm.pinstrLabel bElse
  modify (\s -> s {stackset = stacksetBack})
  g (Tail, e2)

gAuxNonTailIf :: Dest -> Asm.T -> Asm.T -> String -> (Asm.Operand -> String) -> MinCamlEmit ()
gAuxNonTailIf dest e1 e2 labelBase bn = do
  bElse <- genIdHelper (labelBase ++ "_else")
  bCont <- genIdHelper (labelBase ++ "_cont")
  out $ bn $ Asm.Lab bElse
  stacksetBack <- fmap stackset get
  g (dest, e1)
  stackset1 <- fmap stackset get
  out $ Asm.instrJmp $ Asm.Lab bCont
  out $ Asm.pinstrLabel bElse
  modify (\s -> s {stackset = stacksetBack})
  g (dest, e2)
  out $ Asm.pinstrLabel bCont
  stackset2 <- fmap stackset get
  modify (\s -> s {stackset = Set.intersection stackset1 stackset2})

gAux :: (Dest, Asm.Exp) -> MinCamlEmit ()
gAux (NonTail _, Asm.Nop) = return ()
gAux (NonTail x, Asm.Set i) = out $ Asm.instrMov (Asm.Reg x) $ Asm.Imm i
gAux (NonTail x, Asm.Mov y)
  | x /= y = out $ Asm.instrMov (Asm.Reg x) $ Asm.Reg y
gAux (NonTail x, Asm.Mov y) = return ()
gAux (NonTail x, Asm.Neg y)
  | x /= y = out (Asm.instrMov (Asm.Reg x) $ Asm.Reg y) >> out (Asm.instrNeg $ Asm.Reg x)
gAux (NonTail x, Asm.Neg y) = out $ Asm.instrNeg $ Asm.Reg x
gAux (NonTail x, Asm.Add y z')
  | Asm.V x == z' = out $ Asm.instrAdd (Asm.Reg x) $ Asm.Reg y
gAux (NonTail x, Asm.Add y z')
  | x /= y = out (Asm.instrMov (Asm.Reg x) $ Asm.Reg y) >> out (Asm.instrAdd (Asm.Reg x) $ ppIdOrImm z')
gAux (NonTail x, Asm.Add y z') = out $ Asm.instrAdd (Asm.Reg x) $ ppIdOrImm z'
gAux (NonTail x, Asm.Sub y z')
  | Asm.V x == z' = out (Asm.instrSub (Asm.Reg x) $ Asm.Reg y) >> out (Asm.instrNeg $ Asm.Reg x)
gAux (NonTail x, Asm.Sub y z')
  | x /= y = out (Asm.instrMov (Asm.Reg x) $ Asm.Reg y) >> out (Asm.instrSub (Asm.Reg x) $ ppIdOrImm z')
gAux (NonTail x, Asm.Sub y z') = out $ Asm.instrSub (Asm.Reg x) $ ppIdOrImm z'
gAux (Tail, exp@Asm.Nop) = gAuxNonRetHelper exp >> out Asm.instrRet
gAux (Tail, exp@(Asm.Set _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Mov _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Neg _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Add _ _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Sub _ _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, Asm.IfEq x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxTailIf e1 e2 "ifeq_tail" Asm.instrJne
gAux (Tail, Asm.IfLe x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxTailIf e1 e2 "ifle_tail" Asm.instrJg
gAux (NonTail z, Asm.IfEq x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxNonTailIf (NonTail z) e1 e2 "ifeq_nontail" Asm.instrJne
gAux (NonTail z, Asm.IfLe x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxNonTailIf (NonTail z) e1 e2 "ifle_nontail" Asm.instrJg

g :: (Dest, Asm.T) -> MinCamlEmit ()
g (dest, Asm.Ans exp)          = gAux (dest, exp)
g (dest, Asm.Let (x, t) exp e) = gAux (NonTail x, exp) >> g (dest, e)

h :: Asm.Fundef -> MinCamlEmit ()
h = undefined

run :: MinCamlEmit a -> EmitStatus -> (a, EmitStatus)
run e s = runIdentity $ runStateT e s

f :: Asm.Prog -> MinCaml ([String], [String], [String])
f (Asm.Prog fdata fundefs e) = do
  gs <- get
  let es = EmitStatus gs [] Set.empty
      (_, es') = run (mapM_ undefined fdata) es
      asmFdata = reverse $ buffer es'
      (_, es'') = run (mapM_ h fundefs) es'
      asmFundefs = reverse $ buffer es''
      (_, es''') = run (g (NonTail Asm.callResultReg, e)) es'
      asmE = reverse $ buffer es'''
      gs' = globalStatus es'''
  put gs'
  return (asmFdata, asmFundefs, asmE)

f' :: Asm.Prog -> MinCaml [String]
f' prog = do
  (asmFdata, asmFundefs, asmE) <- f prog
  return $
    [".data", ".balign 8"] ++
    asmFdata ++
    [".text"] ++
    asmFundefs ++
    [ ".global min_caml_start"
    , Asm.pinstrLabel "min_caml_start"
    , Asm.instrPush $ Asm.Reg Asm.regRax
    , Asm.instrPush $ Asm.Reg Asm.regRdx
    , Asm.instrPush $ Asm.Reg Asm.regRcx
    , Asm.instrPush $ Asm.Reg Asm.regR8
    , Asm.instrPush $ Asm.Reg regX86Rbp
    , Asm.instrMov (Asm.Reg Asm.regSp) $ Asm.Reg Asm.regRdi
    , Asm.instrMov (Asm.Reg Asm.regHp) $ Asm.Reg Asm.regRsi
    ] ++
    asmE ++
    [ Asm.instrPop $ Asm.Reg regX86Rbp
    , Asm.instrPop $ Asm.Reg Asm.regR8
    , Asm.instrPop $ Asm.Reg Asm.regRcx
    , Asm.instrPop $ Asm.Reg Asm.regRdx
    , Asm.instrPop $ Asm.Reg Asm.regRax
    , Asm.instrRet
    ]
