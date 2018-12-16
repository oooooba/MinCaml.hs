module MinCaml.RegAlloc
  ( f
  ) where

import           Control.Applicative        ((<$>))
import           Control.Exception          (assert)
import           Control.Monad              (liftM2)
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

target' :: Id.T -> (Id.T, Type.Type) -> Asm.Exp -> (Bool, [Id.T])
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
source' t (Asm.Add x (Asm.C _)) = [x]
source' t (Asm.Add x (Asm.V y)) = [x, y]
source' _ _                     = []

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
        _         -> Asm.allregs
    allocBody :: () -> AllocResult
    allocBody _
      | null all = Alloc "%unit"
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

gAux :: (Id.T, Type.Type) -> Asm.T -> RegEnv -> Asm.Exp -> MinCamlRegAlloc (Asm.T, RegEnv)
gAux dest cont regenv exp@Asm.Nop = return (Asm.Ans exp, regenv)
gAux dest cont regenv exp@(Asm.Set _) = return (Asm.Ans exp, regenv)
gAux dest cont regenv (Asm.Add x y') = wrapExp regenv <$> liftM2 Asm.Add (find x Type.Int regenv) (find' y' regenv)
gAux dest cont regenv (Asm.Sub x y') = wrapExp regenv <$> liftM2 Asm.Sub (find x Type.Int regenv) (find' y' regenv)

seqHelper :: Asm.Exp -> Asm.T -> MinCamlRegAlloc Asm.T
seqHelper exp e = do
  globalStatus <- get
  let (result, globalStatus') = runMinCaml (Asm.seq (exp, e)) globalStatus
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
h = undefined

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
