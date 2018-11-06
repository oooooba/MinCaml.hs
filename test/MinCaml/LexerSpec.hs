module MinCaml.LexerSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer  as Lexer

specHelper :: String -> [Lexer.Token] -> Spec
specHelper s expected = it s $ Lexer.runLexer s `shouldBe` expected

spec :: Spec
spec = do
  describe "lexing" $ do
    specHelper "true" [Lexer.BOOL True]
    specHelper "false" [Lexer.BOOL False]
    specHelper "(not)" [Lexer.LPAREN, Lexer.NOT, Lexer.RPAREN]
    specHelper "1 23 4" [Lexer.INT 1, Lexer.INT 23, Lexer.INT 4]
    specHelper "1.0 1.25" [Lexer.FLOAT $ read "1.0", Lexer.FLOAT $ read "1.25"]
