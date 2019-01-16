module MinCaml.Emit
  ( f
  , f'
  ) where

import           Control.Applicative        ((<$>))
import           Control.Arrow              ((***))
import           Control.Exception          (assert)
import           Control.Monad              (when)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, modify, put,
                                             runStateT)
import           Data.Either                (fromRight)
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe
import qualified Data.Set                   as Set

import qualified MinCaml.Asm                as Asm
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data EmitStatus = EmitStatus
  { globalStatus :: GlobalStatus
  , buffer       :: [String]
  , stackset     :: Set.Set Id.T
  , stackmap     :: [Id.T]
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

save :: Id.T -> MinCamlEmit ()
save x = do
  es <- get
  let ss' = Set.insert x $ stackset es
      sm = stackmap es
      sm' =
        if x `elem` sm
          then sm
          else sm ++ [x]
  put $ es {stackset = ss', stackmap = sm'}

locate :: Id.T -> MinCamlEmit [Int]
locate x = fmap (loc . stackmap) get
  where
    loc [] = []
    loc (y:zs)
      | x == y = 0 : fmap succ (loc zs)
    loc (y:zs) = fmap succ (loc zs)

offset :: Id.T -> MinCamlEmit Int
offset x = ((8 *) . head) <$> locate x

stacksize :: MinCamlEmit Int
stacksize = fmap (\s -> Asm.align $ length (stackmap s) * 8) get

ppIdOrImm :: Asm.IdOrImm -> Asm.Operand
ppIdOrImm (Asm.V x) = Asm.Reg x
ppIdOrImm (Asm.C i) = Asm.Imm i

shuffle :: Asm.Operand -> [(Asm.Operand, Asm.Operand)] -> [(Asm.Operand, Asm.Operand)]
shuffle tmpSlot xys =
  let (_, xys') = List.partition (uncurry (==)) xys
  in case List.partition (\(_, y) -> Maybe.isJust $ List.lookup y xys') xys' of
       ([], []) -> []
       ((x, y):xys, []) ->
         let xys' =
               fmap
                 (\yz@(y', z) ->
                    if y == y'
                      then (tmpSlot, z)
                      else yz)
                 xys
         in (y, tmpSlot) : (x, y) : shuffle tmpSlot xys'
       (xys, acyc) -> acyc ++ shuffle tmpSlot xys

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

gAuxArgs :: [(Id.T, Id.T)] -> [Id.T] -> [Id.T] -> MinCamlEmit ()
gAuxArgs xRegCl ys zs = do
  assert (length ys <= length Asm.callArgumentRegs - length xRegCl) $ return ()
  assert (length zs <= length Asm.fregs) $ return ()
  let (_, yrs) = foldl (\(i, yrs) y -> (i + 1, (y, Asm.callArgumentRegs !! i) : yrs)) (0, xRegCl) ys
  tmpSlot <- fmap (Asm.Mem Asm.regSp) stacksize
  mapM_ (\(y, r) -> out $ Asm.instrMov r y) (shuffle tmpSlot $ fmap (Asm.Reg *** Asm.Reg) yrs)

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
gAux (NonTail _, Asm.Save x y) = do
  es <- get
  if x `elem` Asm.callArgumentRegs && not (y `Set.member` stackset es)
    then do
      save y
      n <- offset y
      out $ Asm.instrMov (Asm.Mem Asm.regSp n) $ Asm.Reg x
    else assert (y `Set.member` stackset es) $ return ()
gAux (NonTail x, Asm.Restore y)
  | x `elem` Asm.callArgumentRegs = offset y >>= \n -> out $ Asm.instrMov (Asm.Reg x) $ Asm.Mem Asm.regSp n
gAux (Tail, exp@Asm.Nop) = gAuxNonRetHelper exp >> out Asm.instrRet
gAux (Tail, exp@(Asm.Save _ _)) = gAuxNonRetHelper exp >> out Asm.instrRet
gAux (Tail, exp@(Asm.Set _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Mov _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Neg _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Add _ _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Sub _ _)) = gAux (NonTail Asm.callResultReg, exp) >> out Asm.instrRet
gAux (Tail, exp@(Asm.Restore x)) = do
  l <- locate x
  case l of
    [i] -> gAux (NonTail Asm.callResultReg, exp)
  out Asm.instrRet
gAux (Tail, Asm.IfEq x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxTailIf e1 e2 "ifeq_tail" Asm.instrJne
gAux (Tail, Asm.IfLe x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxTailIf e1 e2 "ifle_tail" Asm.instrJg
gAux (NonTail z, Asm.IfEq x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxNonTailIf (NonTail z) e1 e2 "ifeq_nontail" Asm.instrJne
gAux (NonTail z, Asm.IfLe x y' e1 e2) =
  out (Asm.instrCmp (Asm.Reg x) $ ppIdOrImm y') >> gAuxNonTailIf (NonTail z) e1 e2 "ifle_nontail" Asm.instrJg
gAux (Tail, Asm.CallDir (Id.L x) ys zs) = gAuxArgs [] ys zs >> out (Asm.instrJmp $ Asm.Lab x)
gAux (NonTail a, Asm.CallDir (Id.L x) ys zs) = do
  gAuxArgs [] ys zs
  ss <- stacksize
  when (ss > 0) $ out $ Asm.instrAdd (Asm.Reg Asm.regSp) $ Asm.Imm ss
  out $ Asm.instrCall $ Asm.Lab x
  when (ss > 0) $ out $ Asm.instrSub (Asm.Reg Asm.regSp) $ Asm.Imm ss
  when (a /= Asm.callResultReg) $ out $ Asm.instrMov (Asm.Reg Asm.callResultReg) $ Asm.Reg a

g :: (Dest, Asm.T) -> MinCamlEmit ()
g (dest, Asm.Ans exp)          = gAux (dest, exp)
g (dest, Asm.Let (x, t) exp e) = gAux (NonTail x, exp) >> g (dest, e)

h :: Asm.Fundef -> MinCamlEmit ()
h (Asm.Fundef (Id.L x) _ _ e _) = do
  out $ Asm.pinstrLabel x
  modify (\s -> s {stackset = Set.empty, stackmap = []})
  g (Tail, e)

run :: MinCamlEmit a -> EmitStatus -> (a, EmitStatus)
run e s = runIdentity $ runStateT e s

f :: Asm.Prog -> MinCaml ([String], [String], [String])
f (Asm.Prog fdata fundefs e) = do
  gs <- get
  let es = EmitStatus gs [] Set.empty []
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
    , Asm.instrRet
    ]
