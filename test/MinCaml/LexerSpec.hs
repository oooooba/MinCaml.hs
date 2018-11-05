module MinCaml.LexerSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer  as Lexer

spec :: Spec
spec = do
  describe "lexing" $ do
    let s = "true"
    it s $ Lexer.runLexer s `shouldBe` [Lexer.BOOL True]
    let s = "false"
    it s $ Lexer.runLexer s `shouldBe` [Lexer.BOOL False]
    let s = "(not)"
    it s $ Lexer.runLexer s `shouldBe` [Lexer.LPAREN, Lexer.NOT, Lexer.RPAREN]
    let s = "1 23 4"
    it s $ Lexer.runLexer s `shouldBe` [Lexer.INT 1, Lexer.INT 23, Lexer.INT 4]
    let s = "1.0 1.25"
    it s $ Lexer.runLexer s `shouldBe` [Lexer.FLOAT $ read "1.0", Lexer.FLOAT $ read "1.25"]
