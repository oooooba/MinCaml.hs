module MinCaml.LexerSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer    as Lexer

import           MinCaml.TestCase

specHelperOld :: String -> [Lexer.Token] -> Spec
specHelperOld s expected = it s $ Lexer.runLexer s `shouldBe` expected

specHelper :: TestCase -> [Lexer.Token] -> Spec
specHelper testCase expected = it (name testCase) $ Lexer.runLexer (input testCase) `shouldBe` expected

spec :: Spec
spec = do
  describe "lexing" $ do
    specHelperOld "true" [Lexer.BOOL True]
    specHelperOld "false" [Lexer.BOOL False]
    specHelperOld "(not)" [Lexer.LPAREN, Lexer.NOT, Lexer.RPAREN]
    specHelperOld "1 23 4" [Lexer.INT 1, Lexer.INT 23, Lexer.INT 4]
    specHelperOld "1.0 1.25" [Lexer.FLOAT $ read "1.0", Lexer.FLOAT $ read "1.25"]
    specHelperOld "1+2" [Lexer.INT 1, Lexer.PLUS, Lexer.INT 2]
    specHelperOld "-1" [Lexer.MINUS, Lexer.INT 1]
    specHelperOld "-1-2" [Lexer.MINUS, Lexer.INT 1, Lexer.MINUS, Lexer.INT 2]
    specHelperOld "if 1 then 2 else 3" [Lexer.IF, Lexer.INT 1, Lexer.THEN, Lexer.INT 2, Lexer.ELSE, Lexer.INT 3]
  describe "valid cases" $ do
    specHelper validCase1 [Lexer.LPAREN, Lexer.RPAREN]
    specHelper validCase2 [Lexer.LPAREN, Lexer.LPAREN, Lexer.RPAREN, Lexer.RPAREN]
    specHelper validCase3 [Lexer.INT 42]
    specHelper validCase4 [Lexer.LPAREN, Lexer.INT 42, Lexer.RPAREN]
