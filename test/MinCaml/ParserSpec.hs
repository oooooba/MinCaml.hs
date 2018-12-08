module MinCaml.ParserSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Syntax   as Syntax
import qualified MinCaml.Type     as Type

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
    specHelperOld "-1--2" $ Right $ Syntax.Sub (Syntax.Neg (Syntax.Int 1)) (Syntax.Neg (Syntax.Int 2))
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
    specHelper validCase13 $ Right $ Syntax.Let ("x_", Type.Var 0) (Syntax.Int 42) (Syntax.Var "x_")
    specHelper validCase14 $
      Right $ Syntax.Eq (Syntax.Neg $ Syntax.Int 1) (Syntax.Sub (Syntax.Neg $ Syntax.Int 2) (Syntax.Int 3))
    specHelper validCase15 $ Right $ Syntax.If (Syntax.Bool True) (Syntax.Bool False) (Syntax.Not $ Syntax.Bool True)
    specHelper validCase16 $
      Right $
      Syntax.LetRec (Syntax.Fundef ("f", Type.Var 0) [("x", Type.Var 1)] $ Syntax.Add (Syntax.Var "x") (Syntax.Int 1)) $
      Syntax.Int 2
    specHelper validCase17 $
      Right $
      Syntax.LetRec (Syntax.Fundef ("f", Type.Var 0) [("x", Type.Var 1)] $ Syntax.Add (Syntax.Var "x") (Syntax.Int 1)) $
      Syntax.App (Syntax.Var "f") [Syntax.Int 2]
    specHelper validCase18 $
      Right $
      Syntax.LetRec
        (Syntax.Fundef ("f", Type.Var 0) [("x", Type.Var 1), ("y", Type.Var 2)] $
         Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
      Syntax.App (Syntax.Var "f") [Syntax.Int 1, Syntax.Int 2]
    specHelper validCase19 $
      Right $
      Syntax.LetRec
        (Syntax.Fundef ("f", Type.Var 0) [("n", Type.Var 1)] $
         Syntax.If
           (Syntax.Le (Syntax.Var "n") (Syntax.Int 0))
           (Syntax.Int 0)
           (Syntax.Add (Syntax.Var "n") $ Syntax.App (Syntax.Var "f") [Syntax.Sub (Syntax.Var "n") (Syntax.Int 1)])) $
      Syntax.App (Syntax.Var "f") [Syntax.Int 5]
    specHelper validCase20 $
      Right $
      Syntax.LetRec
        (Syntax.Fundef ("f", Type.Var 0) [("x", Type.Var 1)] $
         Syntax.LetRec
           (Syntax.Fundef ("g", Type.Var 2) [("y", Type.Var 3)] $ Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
         Syntax.Var "g") $
      Syntax.App (Syntax.App (Syntax.Var "f") [Syntax.Int 1]) [Syntax.Int 2]
    specHelper validCase21 $
      Right $
      Syntax.Let ("x", Type.Var 0) (Syntax.Int 1) $
      Syntax.LetRec (Syntax.Fundef ("f", Type.Var 1) [("y", Type.Var 2)] $ Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
      Syntax.LetRec (Syntax.Fundef ("g", Type.Var 3) [("z", Type.Var 4)] $ Syntax.Add (Syntax.Var "z") (Syntax.Int 2)) $
      Syntax.App (Syntax.If (Syntax.Eq (Syntax.Int 3) (Syntax.Int 4)) (Syntax.Var "f") (Syntax.Var "g")) [Syntax.Int 5]
