module MinCaml.ClosureSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Closure  as Closure
import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String Closure.Prog -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Typing.f . Parser.runParser . Lexer.runLexer $ input testCase) >>= KNormal.f . fst >>= Alpha.f >>= Closure.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right $ Closure.Prog [] Closure.Unit
    specHelper validCase2 $ Right $ Closure.Prog [] Closure.Unit
    specHelper validCase3 $ Right $ Closure.Prog [] (Closure.Int 42)
    specHelper validCase4 $ Right $ Closure.Prog [] (Closure.Int 42)
    specHelper validCase5 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 1) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 2) $ Closure.Add "Ti0" "Ti1"
    specHelper validCase6 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 3) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 4) $ Closure.Sub "Ti0" "Ti1"
    specHelper validCase7 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 5) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 6) $ Closure.IfEq "Ti0" "Ti1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase8 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 7) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 8) $ Closure.IfEq "Ti0" "Ti1" (Closure.Int 0) (Closure.Int 1)
    specHelper validCase9 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 9) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 10) $ Closure.IfLe "Ti0" "Ti1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase10 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 12) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 11) $ Closure.IfLe "Ti0" "Ti1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase11 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 14) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 13) $ Closure.IfLe "Ti0" "Ti1" (Closure.Int 0) (Closure.Int 1)
    specHelper validCase12 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 15) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 16) $ Closure.IfLe "Ti0" "Ti1" (Closure.Int 0) (Closure.Int 1)
