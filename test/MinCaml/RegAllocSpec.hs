module MinCaml.RegAllocSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Asm      as Asm
import qualified MinCaml.Closure  as Closure
import           MinCaml.Global
import qualified MinCaml.Id       as Id
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.RegAlloc as RegAlloc
import qualified MinCaml.Simm     as Simm
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing
import qualified MinCaml.Virtual  as Virtual

import           MinCaml.TestCase

specHelper :: TestCase -> Either String Asm.Prog -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f >>=
     Virtual.f >>=
     Simm.f >>=
     RegAlloc.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right $ Asm.Prog [] [] (Asm.Ans Asm.Nop)
    specHelper validCase2 $ Right $ Asm.Prog [] [] (Asm.Ans Asm.Nop)
    specHelper validCase3 $ Right $ Asm.Prog [] [] (Asm.Ans $ Asm.Set 42)
    specHelper validCase4 $ Right $ Asm.Prog [] [] (Asm.Ans $ Asm.Set 42)
    specHelper validCase5 $
      Right $ Asm.Prog [] [] $ Asm.Let (Asm.regEax, Type.Int) (Asm.Set 1) $ Asm.Ans $ Asm.Add Asm.regEax (Asm.C 2)
    specHelper validCase6 $
      Right $ Asm.Prog [] [] $ Asm.Let (Asm.regEax, Type.Int) (Asm.Set 3) $ Asm.Ans $ Asm.Sub Asm.regEax (Asm.C 4)
    specHelper validCase7 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 5) $
      Asm.Ans $ Asm.IfEq Asm.regEax (Asm.C 6) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase8 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 7) $
      Asm.Ans $ Asm.IfEq Asm.regEax (Asm.C 8) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase9 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 9) $
      Asm.Ans $ Asm.IfLe Asm.regEax (Asm.C 10) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase10 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 12) $
      Asm.Ans $ Asm.IfLe Asm.regEax (Asm.C 11) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase11 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 14) $
      Asm.Ans $ Asm.IfLe Asm.regEax (Asm.C 13) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase12 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 15) $
      Asm.Ans $ Asm.IfLe Asm.regEax (Asm.C 16) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase13 $
      Right $ Asm.Prog [] [] $ Asm.Let (Asm.regEax, Type.Int) (Asm.Set 42) $ Asm.Ans $ Asm.Mov Asm.regEax
    specHelper validCase14 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 1) $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Neg Asm.regEax) $
      Asm.Let (Asm.regEbx, Type.Int) (Asm.Set 2) $
      Asm.Let (Asm.regEbx, Type.Int) (Asm.Neg Asm.regEbx) $
      Asm.Let (Asm.regEbx, Type.Int) (Asm.Sub Asm.regEbx $ Asm.C 3) $
      Asm.Ans $ Asm.IfEq Asm.regEax (Asm.V Asm.regEbx) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase15 $
      Right $
      Asm.Prog [] [] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 1) $
      Asm.Ans $
      Asm.IfEq
        Asm.regEax
        (Asm.C 0)
        (Asm.Let (Asm.regEax, Type.Int) (Asm.Set 1) $
         Asm.Ans $ Asm.IfEq Asm.regEax (Asm.C 0) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0))
        (Asm.Ans $ Asm.Set 0)
    specHelper validCase16 $
      Right $
      Asm.Prog [] [Asm.Fundef (Id.L "f.0") [Asm.regEax] [] (Asm.Ans $ Asm.Add Asm.regEax $ Asm.C 1) Type.Int] $
      Asm.Ans $ Asm.Set 2
    specHelper validCase17 $
      Right $
      Asm.Prog [] [Asm.Fundef (Id.L "f.0") [Asm.regEax] [] (Asm.Ans $ Asm.Add Asm.regEax $ Asm.C 1) Type.Int] $
      Asm.Let (Asm.regEax, Type.Int) (Asm.Set 2) $ Asm.Ans $ Asm.CallDir (Id.L "f.0") [Asm.regEax] []
