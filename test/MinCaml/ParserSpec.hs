module MinCaml.ParserSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Syntax   as Syntax

import           MinCaml.TestCase

specHelperOld :: String -> Syntax.T -> Spec
specHelperOld s expected = it s $ (Parser.runParser . Lexer.runLexer) s `shouldBe` expected

specHelper :: TestCase -> Syntax.T -> Spec
specHelper testCase expected =
  it (name testCase) $ (Parser.runParser . Lexer.runLexer $ input testCase) `shouldBe` expected

spec :: Spec
spec = do
  describe "parsing" $ do
    specHelperOld "true" $ Syntax.Bool True
    specHelperOld "false" $ Syntax.Bool False
    specHelperOld "-1" $ Syntax.Neg (Syntax.Int 1)
    specHelperOld "-1-2" $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Int 2)
    specHelperOld "-1--2" $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Neg (Syntax.Int 2))
    specHelperOld "1+2=3-4" $
      Syntax.Eq (Syntax.Add (Syntax.Int 1) (Syntax.Int 2)) (Syntax.Sub (Syntax.Int 3) (Syntax.Int 4))
    specHelperOld "if 1 then 2 else 3" $ Syntax.If (Syntax.Int 1) (Syntax.Int 2) (Syntax.Int 3)
  describe "valid cases" $ do
    specHelper validCase1 Syntax.Unit
    specHelper validCase2 Syntax.Unit
    specHelper validCase3 $ Syntax.Int 42
    specHelper validCase4 $ Syntax.Int 42
    specHelper validCase5 $ Syntax.Add (Syntax.Int 1) (Syntax.Int 2)
    specHelper validCase6 $ Syntax.Sub (Syntax.Int 3) (Syntax.Int 4)
    specHelper validCase7 $ Syntax.Eq (Syntax.Int 5) (Syntax.Int 6)
    specHelper validCase8 $ Syntax.Not $ Syntax.Eq (Syntax.Int 7) (Syntax.Int 8)
    specHelper validCase9 $ Syntax.Le (Syntax.Int 9) (Syntax.Int 10)
    specHelper validCase10 $ Syntax.Le (Syntax.Int 12) (Syntax.Int 11)
    specHelper validCase11 $ Syntax.Not $ Syntax.Le (Syntax.Int 14) (Syntax.Int 13)
    specHelper validCase12 $ Syntax.Not $ Syntax.Le (Syntax.Int 15) (Syntax.Int 16)
