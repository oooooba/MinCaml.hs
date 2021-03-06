module MinCaml.RegAlloc
  ( f
  ) where

import           Control.Applicative        ((<$>))
import           Control.Exception          (assert)
import           Control.Monad              (liftM2, liftM3, liftM4)
import           Control.Monad.Except       (ExceptT, runExceptT, throwError)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Data.Either                (fromRight)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

import qualified MinCaml.Asm                as Asm
import           MinCaml.Global
import qualified MinCaml.Id                 as Id
import qualified MinCaml.Type               as Type

data Exc =
  NoReg Id.T
        Type.Type
  deriving (Show, Eq)

type MinCamlRegAlloc a = StateT GlobalStatus (ExceptT Exc Identity) a

type RegEnv = Map.Map Id.T Id.T

data AllocResult
  = Alloc Id.T
  | Spill Id.T
  deriving (Show, Eq)

targetHelper :: Id.T -> (Id.T, Type.Type) -> Asm.T -> Asm.T -> (Bool, [Id.T])
targetHelper src (dest, t) e1 e2 =
  let (c1, rs1) = target src (dest, t) e1
      (c2, rs2) = target src (dest, t) e2
  in (c1 && c2, rs1 ++ rs2)

targetArgs :: Id.T -> [Id.T] -> Int -> [Id.T] -> [Id.T]
targetArgs _ _ _ [] = []
targetArgs src all n (y:ys)
  | src == y = all !! n : targetArgs src all (n + 1) ys
targetArgs src all n (_:ys) = targetArgs src all (n + 1) ys

target' :: Id.T -> (Id.T, Type.Type) -> Asm.Exp -> (Bool, [Id.T])
target' src (dest, t) (Asm.Mov x)
  | x == src && Asm.isReg dest = assert (t /= Type.Unit) (False, [dest])
target' src (dest, t) (Asm.IfEq _ _ e1 e2) = targetHelper src (dest, t) e1 e2
target' src (dest, t) (Asm.IfLe _ _ e1 e2) = targetHelper src (dest, t) e1 e2
target' src (dest, t) (Asm.CallCls x ys zs) =
  (True, targetArgs src Asm.callArgumentRegs 0 ys ++ targetArgs src Asm.fregs 0 zs ++ [Asm.regCl | x == src])
target' src (dest, t) (Asm.CallDir _ ys zs) =
  (True, targetArgs src Asm.callArgumentRegs 0 ys ++ targetArgs src Asm.fregs 0 zs)
target' _ _ _ = (False, [])

target :: Id.T -> (Id.T, Type.Type) -> Asm.T -> (Bool, [Id.T])
target src dest (Asm.Ans exp) = target' src dest exp
target src dest (Asm.Let xt exp e) =
  let (c1, rs1) = target' src xt exp
  in if c1
       then (True, rs1)
       else let (c2, rs2) = target src dest e
            in (c2, rs1 ++ rs2)

source' :: Type.Type -> Asm.Exp -> [Id.T]
source' _ (Asm.Mov x) = [x]
source' _ (Asm.Neg x) = [x]
source' _ (Asm.Add x (Asm.C _)) = [x]
source' _ (Asm.Add x (Asm.V y)) = [x, y]
source' _ (Asm.Sub x (Asm.C _)) = [x]
source' t (Asm.IfEq _ _ e1 e2) = source t e1 ++ source t e2
source' t (Asm.IfLe _ _ e1 e2) = source t e1 ++ source t e2
source' t Asm.CallDir {} =
  case t of
    Type.Unit -> []
    _         -> [Asm.callResultReg]
    --_         -> [head Asm.callArgumentRegs]
source' _ _ = []

source :: Type.Type -> Asm.T -> [Id.T]
source t (Asm.Ans exp)   = source' t exp
source t (Asm.Let _ _ e) = source t e

alloc :: Asm.T -> RegEnv -> Id.T -> Type.Type -> [Id.T] -> AllocResult
alloc cont regenv x t prefer = assert (not (Map.member x regenv)) $ allocBody ()
  where
    all :: [Id.T]
    all =
      case t of
        Type.Unit -> []
        _         -> Asm.allocatableRegs
    allocBody :: () -> AllocResult
    allocBody _
      | null all = Alloc Asm.regVoid
    allocBody _
      | Asm.isReg x = Alloc x
    allocBody _ =
      let free = Asm.fv cont
          live = foldl collectLiveRegs Set.empty free
          rs = filter (\r -> not $ Set.member r live) $ prefer ++ all
      in case rs of
           r:_ -> Alloc r
           [] ->
             let y = head $ filter isInRegEnv $ reverse free
             in Spill y
    collectLiveRegs :: Set.Set Id.T -> Id.T -> Set.Set Id.T
    collectLiveRegs live y
      | Asm.isReg y = Set.insert y live
    collectLiveRegs live y
      | Map.member y regenv = Set.insert (regenv Map.! y) live
    collectLiveRegs live _ = live
    isInRegEnv :: Id.T -> Bool
    isInRegEnv y
      | Asm.isReg y = False
    isInRegEnv y
      | Map.member y regenv = (regenv Map.! y) `elem` all
    isInRetEnv _ = False

add :: Id.T -> Id.T -> RegEnv -> RegEnv
add x r regenv
  | Asm.isReg x = assert (x == r) regenv
add x r regenv = Map.insert x r regenv

find :: Id.T -> Type.Type -> RegEnv -> MinCamlRegAlloc Id.T
find x _ regenv
  | Asm.isReg x = return x
find x _ regenv
  | Map.member x regenv = return $ regenv Map.! x
find x t _ = throwError $ NoReg x t

find' :: Asm.IdOrImm -> RegEnv -> MinCamlRegAlloc Asm.IdOrImm
find' (Asm.V x) regenv = Asm.V <$> find x Type.Int regenv
find' c _              = return c

wrap :: RegEnv -> Asm.T -> (Asm.T, RegEnv)
wrap = flip (,)

wrapExp :: RegEnv -> Asm.Exp -> (Asm.T, RegEnv)
wrapExp regenv = wrap regenv . Asm.Ans

gAuxAndRestore :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.Exp -> MinCamlRegAlloc (Asm.T, RegEnv)
gAuxAndRestore dest cont regenv exp = do
  globalStatus <- get
  case run (gAux dest cont regenv exp) globalStatus of
    Left (NoReg x t) -> g dest cont regenv $ Asm.Let (x, t) (Asm.Restore x) $ Asm.Ans exp
    Right (result, globalStatus') -> put globalStatus' >> return result

gAuxIf ::
     (Id.T, Type.Type)
  -> Asm.T
  -> RegEnv
  -> Asm.Exp
  -> (Asm.T -> Asm.T -> MinCamlRegAlloc Asm.Exp)
  -> Asm.T
  -> Asm.T
  -> MinCamlRegAlloc (Asm.T, RegEnv)
gAuxIf dest cont regenv exp constr e1 e2 = do
  (e1', regenv1) <- g dest cont regenv e1
  (e2', regenv2) <- g dest cont regenv e2
  let regenv' = foldl (extractRegEnv regenv1 regenv2) Map.empty $ Asm.fv cont
  wrap regenv' <$>
    foldl
      (\e x ->
         if x == fst dest || not (Map.member x regenv) || Map.member x regenv'
           then e
           else e >>= seqHelper (Asm.Save (regenv Map.! x) x))
      (Asm.Ans <$> constr e1' e2')
      (Asm.fv cont)
  where
    extractRegEnv :: RegEnv -> RegEnv -> RegEnv -> Id.T -> RegEnv
    extractRegEnv _ _ regenv' x
      | Asm.isReg x = regenv'
    extractRegEnv regenv1 regenv2 regenv' x
      | Map.member x regenv1 && Map.member x regenv2 =
        let r1 = regenv1 Map.! x
            r2 = regenv2 Map.! x
        in if r1 == r2
             then Map.insert x r1 regenv'
             else regenv'
    extractRegEnv _ _ regenv' _ = regenv'

gAuxCall ::
     (Id.T, Type.Type)
  -> Asm.T
  -> RegEnv
  -> Asm.Exp
  -> ([Id.T] -> [Id.T] -> Asm.Exp)
  -> [Id.T]
  -> [Id.T]
  -> MinCamlRegAlloc (Asm.T, RegEnv)
gAuxCall dest cont regenv exp constr ys zs = do
  ys' <- mapM (\y -> find y Type.Int regenv) ys
  zs' <- mapM (const undefined) zs
  e <-
    foldl
      (\e x ->
         if x == fst dest || x `Map.notMember` regenv
           then e
           else e >>= seqHelper (Asm.Save (regenv Map.! x) x))
      (return $ Asm.Ans $ constr ys' zs') $
    Asm.fv cont
  return (e, Map.empty)

gAux :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.Exp -> MinCamlRegAlloc (Asm.T, RegEnv)
gAux dest cont regenv exp@Asm.Nop = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.Set _) = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.SetL _) = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.Restore _) = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.Mov x) = wrapExp regenv . Asm.Mov <$> find x Type.Int regenv
gAux dest cont regenv exp@(Asm.Neg x) = wrapExp regenv . Asm.Neg <$> find x Type.Int regenv
gAux dest cont regenv (Asm.Add x y') = wrapExp regenv <$> liftM2 Asm.Add (find x Type.Int regenv) (find' y' regenv)
gAux dest cont regenv (Asm.Sub x y') = wrapExp regenv <$> liftM2 Asm.Sub (find x Type.Int regenv) (find' y' regenv)
gAux dest cont regenv (Asm.Ld x y' i) =
  wrapExp regenv <$> liftM3 Asm.Ld (find x Type.Int regenv) (find' y' regenv) (return i)
gAux dest cont regenv (Asm.St x y z' i) =
  wrapExp regenv <$> liftM4 Asm.St (find x Type.Int regenv) (find y Type.Int regenv) (find' z' regenv) (return i)
gAux dest cont regenv exp@(Asm.IfEq x y' e1 e2) =
  gAuxIf dest cont regenv exp (gAuxIfHelper Asm.IfEq Type.Int regenv x y') e1 e2
gAux dest cont regenv exp@(Asm.IfLe x y' e1 e2) =
  gAuxIf dest cont regenv exp (gAuxIfHelper Asm.IfLe Type.Int regenv x y') e1 e2
gAux dest cont regenv exp@(Asm.CallCls x ys zs) =
  if length ys > length Asm.callArgumentRegs - 1 || length zs > length Asm.fregs
    then error $ "cannot allocate registers for arguments to " ++ show x
    else find x Type.Int regenv >>= \r -> gAuxCall dest cont regenv exp (Asm.CallCls r) ys zs
gAux dest cont regenv exp@(Asm.CallDir (Id.L x) ys zs) =
  if length ys > length Asm.callArgumentRegs || length zs > length Asm.fregs
    then error $ "cannot allocate registers for arguments to " ++ show x
    else gAuxCall dest cont regenv exp (Asm.CallDir $ Id.L x) ys zs
gAux _ _ _ (Asm.Save _ _) = error "never reached"

gAuxIfHelper ::
     (Id.T -> Asm.IdOrImm -> Asm.T -> Asm.T -> Asm.Exp)
  -> Type.Type
  -> RegEnv
  -> Id.T
  -> Asm.IdOrImm
  -> Asm.T
  -> Asm.T
  -> MinCamlRegAlloc Asm.Exp
gAuxIfHelper c t regenv x y' e1' e2' = do
  rx <- find x t regenv
  ry' <- find' y' regenv
  return $ c rx ry' e1' e2'

seqHelper :: Asm.Exp -> Asm.T -> MinCamlRegAlloc Asm.T
seqHelper exp e = do
  globalStatus <- get
  let (result, globalStatus') = runMinCaml (Asm.seq (exp, e)) globalStatus
  put globalStatus'
  return $ fromRight undefined result

genVarHelper :: Type.Type -> MinCamlRegAlloc Id.T
genVarHelper t = do
  globalStatus <- get
  let (result, globalStatus') = runMinCaml (genVar t) globalStatus
  put globalStatus'
  return $ fromRight undefined result

g :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.T -> MinCamlRegAlloc (Asm.T, RegEnv)
g dest cont regenv (Asm.Ans exp) = gAuxAndRestore dest cont regenv exp
g dest cont regenv (Asm.Let xt@(x, t) exp e) = do
  assert (not $ Map.member x regenv) $ return ()
  let cont' = Asm.concat e dest cont
  (e1', regenv1) <- gAuxAndRestore xt cont' regenv exp
  let (_, targets) = target x dest cont'
  let sources = source t e1'
  case alloc cont' regenv1 x t $ targets ++ sources of
    Spill y -> do
      let r = regenv1 Map.! y
      (e2', regenv2) <- g dest cont (add x r (Map.delete y regenv1)) e
      let save =
            case Map.lookup y regenv of
              Just r  -> Asm.Save r y
              Nothing -> Asm.Nop
      fmap (wrap regenv2) $ seqHelper save $ Asm.concat e1' (r, t) e2'
    Alloc r -> do
      (e2', regenv2) <- g dest cont (add x r regenv1) e
      return (Asm.concat e1' (r, t) e2', regenv2)

h :: Asm.Fundef -> MinCamlRegAlloc Asm.Fundef
h (Asm.Fundef (Id.L x) ys zs e t) = do
  let regenv = Map.insert x Asm.regCl Map.empty
  let (i, argRegs, regenv') =
        foldl
          (\(i, argRegs, regenv) y ->
             let r = Asm.callArgumentRegs !! i
             in (i + 1, argRegs ++ [r], assert (not $ Asm.isReg y) $ Map.insert y r regenv))
          (0, [], regenv)
          ys
  let (_, fargRegs, regenv'') = (undefined, [], regenv')
  a <-
    case t of
      Type.Unit -> genVarHelper Type.Unit
      _         -> return Asm.callResultReg
  (e', _) <- g (a, t) (Asm.Ans $ Asm.Mov a) regenv'' e
  return $ Asm.Fundef (Id.L x) argRegs fargRegs e' t

run :: MinCamlRegAlloc a -> GlobalStatus -> Either Exc (a, GlobalStatus)
run e s = runIdentity (runExceptT $ runStateT e s)

f :: Asm.Prog -> MinCaml Asm.Prog
f (Asm.Prog fdata fundefs e) = do
  tmp <- genVar Type.Unit
  globalStatus <- get
  let (fundefs', globalStatus') =
        foldl
          (\(fundefs', gs) fundef ->
             let (fundef', gs') = fromRight undefined (run (h fundef) gs)
             in (fundef' : fundefs', gs'))
          ([], globalStatus)
          fundefs
  let ((e', _), globalStatus'') =
        fromRight undefined $ flip run globalStatus' $ g (tmp, Type.Unit) (Asm.Ans Asm.Nop) Map.empty e
  put globalStatus''
  Prelude.return $ Asm.Prog fdata (reverse fundefs') e'
