module MinCaml.KNormalSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String KNormal.T -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right KNormal.Unit
    specHelper validCase2 $ Right KNormal.Unit
    specHelper validCase3 $ Right (KNormal.Int 42)
    specHelper validCase4 $ Right (KNormal.Int 42)
    specHelper validCase5 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0" "Ti1"
    specHelper validCase6 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti0" "Ti1"
    specHelper validCase7 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 6) $ KNormal.IfEq "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase8 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 7) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 8) $ KNormal.IfEq "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase9 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 9) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 10) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase10 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 12) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 11) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase11 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 14) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 13) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase12 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 15) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 16) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
