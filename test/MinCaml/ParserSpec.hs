module MinCaml.ParserSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Lexer  as Lexer
import qualified MinCaml.Parser as Parser
import qualified MinCaml.Syntax as Syntax

specHelper :: String -> Syntax.T -> Spec
specHelper s expected = it s $ (Parser.runParser . Lexer.runLexer) s `shouldBe` expected

spec :: Spec
spec =
  describe "parsing" $ do
    specHelper "()" Syntax.Unit
    specHelper "true" $ Syntax.Bool True
    specHelper "false" $ Syntax.Bool False
    specHelper "1" $ Syntax.Int 1
    specHelper "(())" Syntax.Unit
    specHelper "1+2" $ Syntax.Add (Syntax.Int 1) (Syntax.Int 2)
    specHelper "-1" $ Syntax.Neg (Syntax.Int 1)
    specHelper "-1-2" $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Int 2)
    specHelper "-1--2" $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Neg (Syntax.Int 2))
    specHelper "1+2=3-4" $
      Syntax.Eq (Syntax.Add (Syntax.Int 1) (Syntax.Int 2)) (Syntax.Sub (Syntax.Int 3) (Syntax.Int 4))
    specHelper "if 1 then 2 else 3" $ Syntax.If (Syntax.Int 1) (Syntax.Int 2) (Syntax.Int 3)
