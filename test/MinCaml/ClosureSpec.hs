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
