module MinCaml.SimmSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Asm      as Asm
import qualified MinCaml.Closure  as Closure
import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
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
     Simm.f)
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
      Right $ Asm.Prog [] [] $ Asm.Let ("Ti0.0", Type.Int) (Asm.Set 1) $ Asm.Ans $ Asm.Add "Ti0.0" (Asm.C 2)
    specHelper validCase6 $
      Right $ Asm.Prog [] [] $ Asm.Let ("Ti0.0", Type.Int) (Asm.Set 3) $ Asm.Ans $ Asm.Sub "Ti0.0" (Asm.C 4)
    specHelper validCase7 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 5) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.C 6) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase8 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 7) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.C 8) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase9 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 9) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 10) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase10 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 12) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 11) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase11 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 14) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 13) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase12 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 15) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 16) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)