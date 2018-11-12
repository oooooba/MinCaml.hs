module MinCaml.KNormalSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String KNormal.T -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml ((Typing.f . Parser.runParser . Lexer.runLexer $ input testCase) >>= KNormal.f . fst) initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right KNormal.Unit
    specHelper validCase2 $ Right KNormal.Unit
    specHelper validCase3 $ Right (KNormal.Int 42)
    specHelper validCase4 $ Right (KNormal.Int 42)
