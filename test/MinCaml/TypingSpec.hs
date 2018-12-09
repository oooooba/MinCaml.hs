module MinCaml.TypingSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Syntax   as Syntax
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

typing :: Syntax.T -> MinCaml (Syntax.T, Type.Type)
typing e = do
  t <- genType
  let topLevel = Syntax.Let ("_", t) e Syntax.Unit
  Typing.f topLevel

expected :: Type.Type -> Syntax.T -> Syntax.T
expected t e = Syntax.Let ("_", t) e Syntax.Unit

specHelper :: TestCase -> Either String (Syntax.T, Type.Type) -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f) initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec = do
  describe "typing" $ do
    it "unit literal" $
      evalMinCaml (typing Syntax.Unit) initialGlobalStatus `shouldBe` Right (expected Type.Unit Syntax.Unit, Type.Unit)
    it "integer literal with let" $
      evalMinCaml (typing $ Syntax.Int 42) initialGlobalStatus `shouldBe`
      Right (expected Type.Int (Syntax.Int 42), Type.Unit)
    it "integer literal" $
      evalMinCaml (Typing.f $ Syntax.Int 42) initialGlobalStatus `shouldBe` Right (Syntax.Int 42, Type.Int)
    it "integer add" $
      evalMinCaml
        (typing =<< do
           t1 <- genType
           t2 <- genType
           return $
             Syntax.Let
               ("x", t1)
               (Syntax.Int 1)
               (Syntax.Let ("y", t2) (Syntax.Int 2) (Syntax.Add (Syntax.Var "x") (Syntax.Var "y"))))
        initialGlobalStatus `shouldBe`
      Right
        ( expected
            Type.Int
            (Syntax.Let
               ("x", Type.Int)
               (Syntax.Int 1)
               (Syntax.Let ("y", Type.Int) (Syntax.Int 2) (Syntax.Add (Syntax.Var "x") (Syntax.Var "y"))))
        , Type.Unit)
  describe "valid cases" $ do
    specHelper validCase1 $ Right (Syntax.Unit, Type.Unit)
    specHelper validCase2 $ Right (Syntax.Unit, Type.Unit)
    specHelper validCase3 $ Right (Syntax.Int 42, Type.Int)
    specHelper validCase4 $ Right (Syntax.Int 42, Type.Int)
    specHelper validCase5 $ Right (Syntax.Add (Syntax.Int 1) (Syntax.Int 2), Type.Int)
    specHelper validCase6 $ Right (Syntax.Sub (Syntax.Int 3) (Syntax.Int 4), Type.Int)
    specHelper validCase7 $ Right (Syntax.Eq (Syntax.Int 5) (Syntax.Int 6), Type.Bool)
    specHelper validCase8 $ Right (Syntax.Not $ Syntax.Eq (Syntax.Int 7) (Syntax.Int 8), Type.Bool)
    specHelper validCase9 $ Right (Syntax.Le (Syntax.Int 9) (Syntax.Int 10), Type.Bool)
    specHelper validCase10 $ Right (Syntax.Le (Syntax.Int 12) (Syntax.Int 11), Type.Bool)
    specHelper validCase11 $ Right (Syntax.Not $ Syntax.Le (Syntax.Int 14) (Syntax.Int 13), Type.Bool)
    specHelper validCase12 $ Right (Syntax.Not $ Syntax.Le (Syntax.Int 15) (Syntax.Int 16), Type.Bool)
    specHelper validCase13 $ Right (Syntax.Let ("x_", Type.Int) (Syntax.Int 42) (Syntax.Var "x_"), Type.Int)
    specHelper validCase14 $
      Right (Syntax.Eq (Syntax.Neg $ Syntax.Int 1) (Syntax.Sub (Syntax.Neg $ Syntax.Int 2) (Syntax.Int 3)), Type.Bool)
    specHelper validCase15 $
      Right (Syntax.If (Syntax.Bool True) (Syntax.Bool False) (Syntax.Not $ Syntax.Bool True), Type.Bool)
    specHelper validCase16 $
      Right
        ( Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
             Syntax.Add (Syntax.Var "x") (Syntax.Int 1)) $
          Syntax.Int 2
        , Type.Int)
    specHelper validCase17 $
      Right
        ( Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
             Syntax.Add (Syntax.Var "x") (Syntax.Int 1)) $
          Syntax.App (Syntax.Var "f") [Syntax.Int 2]
        , Type.Int)
    specHelper validCase18 $
      Right
        ( Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int, Type.Int] Type.Int) [("x", Type.Int), ("y", Type.Int)] $
             Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
          Syntax.App (Syntax.Var "f") [Syntax.Int 1, Syntax.Int 2]
        , Type.Int)
    specHelper validCase19 $
      Right
        ( Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("n", Type.Int)] $
             Syntax.If
               (Syntax.Le (Syntax.Var "n") (Syntax.Int 0))
               (Syntax.Int 0)
               (Syntax.Add (Syntax.Var "n") $ Syntax.App (Syntax.Var "f") [Syntax.Sub (Syntax.Var "n") (Syntax.Int 1)])) $
          Syntax.App (Syntax.Var "f") [Syntax.Int 5]
        , Type.Int)
    specHelper validCase20 $
      Right
        ( Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int] $ Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
             Syntax.LetRec
               (Syntax.Fundef ("g", Type.Fun [Type.Int] Type.Int) [("y", Type.Int)] $
                Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
             Syntax.Var "g") $
          Syntax.App (Syntax.App (Syntax.Var "f") [Syntax.Int 1]) [Syntax.Int 2]
        , Type.Int)
    specHelper validCase21 $
      Right
        ( Syntax.Let ("x", Type.Int) (Syntax.Int 1) $
          Syntax.LetRec
            (Syntax.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("y", Type.Int)] $
             Syntax.Add (Syntax.Var "x") (Syntax.Var "y")) $
          Syntax.LetRec
            (Syntax.Fundef ("g", Type.Fun [Type.Int] Type.Int) [("z", Type.Int)] $
             Syntax.Add (Syntax.Var "z") (Syntax.Int 2)) $
          Syntax.App
            (Syntax.If (Syntax.Eq (Syntax.Int 3) (Syntax.Int 4)) (Syntax.Var "f") (Syntax.Var "g"))
            [Syntax.Int 5]
        , Type.Int)
    specHelper validCase22 $
      Right (Syntax.Add (Syntax.Add (Syntax.Add (Syntax.Int 1) $ Syntax.Int 2) $ Syntax.Int 3) $ Syntax.Int 4, Type.Int)
    specHelper validCase23 $
      Right (Syntax.Add (Syntax.Sub (Syntax.Int 1) $ Syntax.Int 2) $ Syntax.Sub (Syntax.Int 3) $ Syntax.Int 4, Type.Int)
    specHelper validCase24 $
      Right (Syntax.Add (Syntax.Add (Syntax.Int 1) $ Syntax.Sub (Syntax.Int 2) $ Syntax.Int 3) $ Syntax.Int 4, Type.Int)
