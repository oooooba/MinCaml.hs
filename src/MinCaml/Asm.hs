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

regEax :: String
regEax = "%eax"

regEbx :: String
regEbx = "%ebx"

regEcx :: String
regEcx = "%ecx"

regEdx :: String
regEdx = "%edx"

regEsi :: String
regEsi = "%esi"

regEdi :: String
regEdi = "%edi"

regSp :: String
regSp = "%ebp"

callArgumentRegs :: [Id.T]
callArgumentRegs = [regEbx, regEcx, regEdx, regEsi, regEdi]

callResultReg :: Id.T
callResultReg = regEax

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
    else i + 4
