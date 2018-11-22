module MinCaml.ParserSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Syntax   as Syntax

import           MinCaml.TestCase

specHelperOld :: String -> Either String Syntax.T -> Spec
specHelperOld s expected =
  it s $ evalMinCaml (Parser.runParser . Lexer.runLexer $ s) initialGlobalStatus `shouldBe` expected

specHelper :: TestCase -> Either String Syntax.T -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml (Parser.runParser . Lexer.runLexer $ input testCase) initialGlobalStatus `shouldBe` expected

spec :: Spec
spec = do
  describe "parsing" $ do
    specHelperOld "true" $ Right $ Syntax.Bool True
    specHelperOld "false" $ Right $ Syntax.Bool False
    specHelperOld "-1" $ Right $ Syntax.Neg (Syntax.Int 1)
    specHelperOld "-1-2" $ Right $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Int 2)
    specHelperOld "-1--2" $ Right $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Neg (Syntax.Int 2))
    specHelperOld "1+2=3-4" $
      Right $ Syntax.Eq (Syntax.Add (Syntax.Int 1) (Syntax.Int 2)) (Syntax.Sub (Syntax.Int 3) (Syntax.Int 4))
    specHelperOld "if 1 then 2 else 3" $ Right $ Syntax.If (Syntax.Int 1) (Syntax.Int 2) (Syntax.Int 3)
  describe "valid cases" $ do
    specHelper validCase1 $ Right Syntax.Unit
    specHelper validCase2 $ Right Syntax.Unit
    specHelper validCase3 $ Right $ Syntax.Int 42
    specHelper validCase4 $ Right $ Syntax.Int 42
    specHelper validCase5 $ Right $ Syntax.Add (Syntax.Int 1) (Syntax.Int 2)
    specHelper validCase6 $ Right $ Syntax.Sub (Syntax.Int 3) (Syntax.Int 4)
    specHelper validCase7 $ Right $ Syntax.Eq (Syntax.Int 5) (Syntax.Int 6)
    specHelper validCase8 $ Right $ Syntax.Not $ Syntax.Eq (Syntax.Int 7) (Syntax.Int 8)
    specHelper validCase9 $ Right $ Syntax.Le (Syntax.Int 9) (Syntax.Int 10)
    specHelper validCase10 $ Right $ Syntax.Le (Syntax.Int 12) (Syntax.Int 11)
    specHelper validCase11 $ Right $ Syntax.Not $ Syntax.Le (Syntax.Int 14) (Syntax.Int 13)
    specHelper validCase12 $ Right $ Syntax.Not $ Syntax.Le (Syntax.Int 15) (Syntax.Int 16)
