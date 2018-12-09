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
    specHelperOld "if 1 then 2 else 3" [Lexer.IF, Lexer.INT 1, Lexer.THEN, Lexer.INT 2, Lexer.ELSE, Lexer.INT 3]
  describe "valid cases" $ do
    specHelper validCase1 [Lexer.LPAREN, Lexer.RPAREN]
    specHelper validCase2 [Lexer.LPAREN, Lexer.LPAREN, Lexer.RPAREN, Lexer.RPAREN]
    specHelper validCase3 [Lexer.INT 42]
    specHelper validCase4 [Lexer.LPAREN, Lexer.INT 42, Lexer.RPAREN]
    specHelper validCase5 [Lexer.INT 1, Lexer.PLUS, Lexer.INT 2]
    specHelper validCase6 [Lexer.INT 3, Lexer.MINUS, Lexer.INT 4]
    specHelper validCase7 [Lexer.INT 5, Lexer.EQUAL, Lexer.INT 6]
    specHelper validCase8 [Lexer.INT 7, Lexer.LESS_GREATER, Lexer.INT 8]
    specHelper validCase9 [Lexer.INT 9, Lexer.LESS_EQUAL, Lexer.INT 10]
    specHelper validCase10 [Lexer.INT 11, Lexer.GREATER_EQUAL, Lexer.INT 12]
    specHelper validCase11 [Lexer.INT 13, Lexer.LESS, Lexer.INT 14]
    specHelper validCase12 [Lexer.INT 15, Lexer.GREATER, Lexer.INT 16]
    specHelper validCase13 [Lexer.LET, Lexer.IDENT "x_", Lexer.EQUAL, Lexer.INT 42, Lexer.IN, Lexer.IDENT "x_"]
    specHelper validCase14 [Lexer.MINUS, Lexer.INT 1, Lexer.EQUAL, Lexer.MINUS, Lexer.INT 2, Lexer.MINUS, Lexer.INT 3]
    specHelper
      validCase15
      [Lexer.IF, Lexer.BOOL True, Lexer.THEN, Lexer.BOOL False, Lexer.ELSE, Lexer.NOT, Lexer.BOOL True]
    specHelper
      validCase16
      [ Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "x"
      , Lexer.EQUAL
      , Lexer.IDENT "x"
      , Lexer.PLUS
      , Lexer.INT 1
      , Lexer.IN
      , Lexer.INT 2
      ]
    specHelper
      validCase17
      [ Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "x"
      , Lexer.EQUAL
      , Lexer.IDENT "x"
      , Lexer.PLUS
      , Lexer.INT 1
      , Lexer.IN
      , Lexer.IDENT "f"
      , Lexer.INT 2
      ]
    specHelper
      validCase18
      [ Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "x"
      , Lexer.IDENT "y"
      , Lexer.EQUAL
      , Lexer.IDENT "x"
      , Lexer.PLUS
      , Lexer.IDENT "y"
      , Lexer.IN
      , Lexer.IDENT "f"
      , Lexer.INT 1
      , Lexer.INT 2
      ]
    specHelper
      validCase19
      [ Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "n"
      , Lexer.EQUAL
      , Lexer.IF
      , Lexer.IDENT "n"
      , Lexer.LESS_EQUAL
      , Lexer.INT 0
      , Lexer.THEN
      , Lexer.INT 0
      , Lexer.ELSE
      , Lexer.IDENT "n"
      , Lexer.PLUS
      , Lexer.IDENT "f"
      , Lexer.LPAREN
      , Lexer.IDENT "n"
      , Lexer.MINUS
      , Lexer.INT 1
      , Lexer.RPAREN
      , Lexer.IN
      , Lexer.IDENT "f"
      , Lexer.INT 5
      ]
    specHelper
      validCase20
      [ Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "x"
      , Lexer.EQUAL
      , Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "g"
      , Lexer.IDENT "y"
      , Lexer.EQUAL
      , Lexer.IDENT "x"
      , Lexer.PLUS
      , Lexer.IDENT "y"
      , Lexer.IN
      , Lexer.IDENT "g"
      , Lexer.IN
      , Lexer.LPAREN
      , Lexer.IDENT "f"
      , Lexer.INT 1
      , Lexer.RPAREN
      , Lexer.INT 2
      ]
    specHelper
      validCase21
      [ Lexer.LET
      , Lexer.IDENT "x"
      , Lexer.EQUAL
      , Lexer.INT 1
      , Lexer.IN
      , Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "f"
      , Lexer.IDENT "y"
      , Lexer.EQUAL
      , Lexer.IDENT "x"
      , Lexer.PLUS
      , Lexer.IDENT "y"
      , Lexer.IN
      , Lexer.LET
      , Lexer.REC
      , Lexer.IDENT "g"
      , Lexer.IDENT "z"
      , Lexer.EQUAL
      , Lexer.IDENT "z"
      , Lexer.PLUS
      , Lexer.INT 2
      , Lexer.IN
      , Lexer.LPAREN
      , Lexer.IF
      , Lexer.INT 3
      , Lexer.EQUAL
      , Lexer.INT 4
      , Lexer.THEN
      , Lexer.IDENT "f"
      , Lexer.ELSE
      , Lexer.IDENT "g"
      , Lexer.RPAREN
      , Lexer.INT 5
      ]
    specHelper validCase22 [Lexer.INT 1, Lexer.PLUS, Lexer.INT 2, Lexer.PLUS, Lexer.INT 3, Lexer.PLUS, Lexer.INT 4]
    specHelper
      validCase23
      [ Lexer.LPAREN
      , Lexer.INT 1
      , Lexer.MINUS
      , Lexer.INT 2
      , Lexer.RPAREN
      , Lexer.PLUS
      , Lexer.LPAREN
      , Lexer.INT 3
      , Lexer.MINUS
      , Lexer.INT 4
      , Lexer.RPAREN
      ]
    specHelper
      validCase24
      [ Lexer.INT 1
      , Lexer.PLUS
      , Lexer.LPAREN
      , Lexer.INT 2
      , Lexer.MINUS
      , Lexer.INT 3
      , Lexer.RPAREN
      , Lexer.PLUS
      , Lexer.INT 4
      ]
