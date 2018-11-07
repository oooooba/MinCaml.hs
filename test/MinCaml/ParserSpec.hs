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
spec = do
  describe "parsing" $ do
    specHelper "()" Syntax.Unit
    specHelper "true" $ Syntax.Bool True
    specHelper "false" $ Syntax.Bool False
    specHelper "1" $ Syntax.Int 1
    specHelper "(())" Syntax.Unit
