module MinCaml.Asm where

import qualified Data.Set       as Set

import           MinCaml.Global
import qualified MinCaml.Id     as Id
import qualified MinCaml.Type   as Type

data IdOrImm
  = V Id.T
  | C Int
  deriving (Show, Eq)

data T
  = Ans Exp
  | Let (Id.T, Type.Type)
        Exp
        T
  deriving (Show, Eq)

data Exp
  = Nop
  | Set Int
  | SetL Id.L
  | Mov Id.T
  | Neg Id.T
  | Add Id.T
        IdOrImm
  | Sub Id.T
        IdOrImm
  | Ld Id.T
       IdOrImm
       Int
  | St Id.T
       Id.T
       IdOrImm
       Int
  | IfEq Id.T
         IdOrImm
         T
         T
  | IfLe Id.T
         IdOrImm
         T
         T
  | CallCls Id.T
            [Id.T]
            [Id.T]
  | CallDir Id.L
            [Id.T]
            [Id.T]
  | Save Id.T
         Id.T
  | Restore Id.T
  deriving (Show, Eq)

data Fundef = Fundef
  { name  :: Id.L
  , args  :: [Id.T]
  , fargs :: [Id.T]
  , body  :: T
  , ret   :: Type.Type
  } deriving (Show, Eq)

data Prog =
  Prog [(Id.L, Float)]
       [Fundef]
       T
  deriving (Show, Eq)

seq :: (Exp, T) -> MinCaml T
seq (e1, e2) = do
  x <- genVar Type.Unit
  return $ Let (x, Type.Unit) e1 e2

regRax :: String
regRax = "%rax"

regRdi :: String
regRdi = "%rdi"

regRsi :: String
regRsi = "%rsi"

regRdx :: String
regRdx = "%rdx"

regRcx :: String
regRcx = "%rcx"

regR8 :: String
regR8 = "%r8"

regSp :: String
regSp = "%rbp"

callArgumentRegs :: [Id.T]
callArgumentRegs = [regRdi, regRsi, regRdx, regRcx, regR8]

callResultReg :: Id.T
callResultReg = regRax

fregs :: [Id.T]
fregs = []

allocatableRegs :: [Id.T]
allocatableRegs = callResultReg : callArgumentRegs

regCl :: String
regCl = last callArgumentRegs

regHp :: String
regHp = "min_caml_hp"

isReg :: Id.T -> Bool
isReg x = head x == '%' || x == regHp

removeAndUniq :: Ord a => Set.Set a -> [a] -> [a]
removeAndUniq _ [] = []
removeAndUniq xs (x:ys)
  | x `Set.member` xs = removeAndUniq xs ys
removeAndUniq xs (x:ys) = x : removeAndUniq (Set.insert x xs) ys

fvIdOrImm :: IdOrImm -> [Id.T]
fvIdOrImm (V x) = [x]
fvIdOrImm _     = []

fvExp :: Exp -> [Id.T]
fvExp Nop = []
fvExp (Set _) = []
fvExp (SetL _) = []
fvExp (Restore _) = []
fvExp (Mov x) = [x]
fvExp (Neg x) = [x]
fvExp (Save x _) = [x]
fvExp (Add x y') = x : fvIdOrImm y'
fvExp (Sub x y') = x : fvIdOrImm y'
fvExp (Ld x y' _) = x : fvIdOrImm y'
fvExp (St x y z' _) = x : y : fvIdOrImm z'
fvExp (IfEq x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fvHelper e1 ++ fvHelper e2)
fvExp (IfLe x y' e1 e2) = x : fvIdOrImm y' ++ removeAndUniq Set.empty (fvHelper e1 ++ fvHelper e2)
fvExp (CallCls x ys zs) = x : ys ++ zs
fvExp (CallDir _ ys zs) = ys ++ zs

fvHelper :: T -> [Id.T]
fvHelper (Ans exp) = fvExp exp
fvHelper (Let (x, _) exp e) = fvExp exp ++ removeAndUniq (Set.singleton x) (fvHelper e)

fv :: T -> [Id.T]
fv e = removeAndUniq Set.empty $ fvHelper e

concat :: T -> (Id.T, Type.Type) -> T -> T
concat (Ans exp) xt e        = Let xt exp e
concat (Let xt exp e1) yt e2 = Let xt exp $ MinCaml.Asm.concat e1 yt e2

align :: Int -> Int
align i =
  if (i `mod` 8) == 0
    then i
    else error $ "found invalid align: " ++ show i

instr0 instr = instr

instr1 instr op1 = instr ++ "\t" ++ op1

instr2 instr op1 op2 = instr ++ "\t" ++ op1 ++ ", " ++ op2

{-
- ToDo: fix to use intel syntax
-}
type Label = String

data Operand
  = Reg Id.T
  | Imm Int
  | Mem Id.T
        Int
  | Lab Label
  deriving (Show, Eq)

instrPush (Reg reg) = instr1 "pushq" reg

instrPop (Reg reg) = instr1 "popq" reg

instrMov (Reg dst) (Reg src)
  | dst == regHp = instr2 "movq" src $ regHp ++ "(%rip)"
instrMov (Reg dst) (Reg src) = instr2 "movq" src dst
instrMov (Reg dst) (Imm imm) = instr2 "movq" ("$" ++ show imm) dst
instrMov (Reg dst) (Mem base offset) = instr2 "movq" (show offset ++ "(" ++ base ++ ")") dst

instrNeg (Reg reg) = instr1 "negq" reg

instrAdd (Reg reg1) (Reg reg2) = instr2 "addq" reg2 reg1
instrAdd (Reg reg) (Imm imm)   = instr2 "addq" ("$" ++ show imm) reg

instrSub (Reg reg1) (Reg reg2) = instr2 "subq" reg2 reg1
instrSub (Reg reg) (Imm imm)   = instr2 "subq" ("$" ++ show imm) reg

instrCmp (Reg reg1) (Reg reg2) = instr2 "cmpq" reg2 reg1
instrCmp (Reg reg) (Imm imm)   = instr2 "cmpq" ("$" ++ show imm) reg

instrJmp (Lab label) = instr1 "jmp" label

instrJne (Lab label) = instr1 "jne" label

instrJg (Lab label) = instr1 "jg" label

instrCall (Lab label) = instr1 "call" label

instrRet = instr0 "ret"

pinstrLabel label = instr0 $ label ++ ":"
