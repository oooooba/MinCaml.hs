module MinCaml.KNormalSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String KNormal.T -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst)
    initialGlobalStatus `shouldBe`
  expected

specHelper2 :: TestCase -> Spec
specHelper2 testCase =
  it (name testCase) $
  runMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f2 . fst)
    initialGlobalStatus `shouldBe`
  runMinCaml ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst) initialGlobalStatus

spec :: Spec
spec = do
  describe "valid cases" $ do
    specHelper validCase1 $ Right KNormal.Unit
    specHelper validCase2 $ Right KNormal.Unit
    specHelper validCase3 $ Right (KNormal.Int 42)
    specHelper validCase4 $ Right (KNormal.Int 42)
    specHelper validCase5 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0" "Ti1"
    specHelper validCase6 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti0" "Ti1"
    specHelper validCase7 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 6) $ KNormal.IfEq "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase8 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 7) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 8) $ KNormal.IfEq "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase9 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 9) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 10) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase10 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 12) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 11) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase11 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 14) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 13) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase12 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 15) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 16) $ KNormal.IfLe "Ti0" "Ti1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase13 $ Right $ KNormal.Let ("x_", Type.Int) (KNormal.Int 42) $ KNormal.Var "x_"
    specHelper validCase14 $
      Right $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $ KNormal.Neg "Ti0") $
      KNormal.Let
        ("Ti5", Type.Int)
        (KNormal.Let ("Ti3", Type.Int) (KNormal.Let ("Ti2", Type.Int) (KNormal.Int 2) $ KNormal.Neg "Ti2") $
         KNormal.Let ("Ti4", Type.Int) (KNormal.Int 3) $ KNormal.Sub "Ti3" "Ti4") $
      KNormal.IfEq "Ti1" "Ti5" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase15 $
      Right $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 0) $
      KNormal.IfEq
        "Ti0"
        "Ti1"
        (KNormal.Let ("Ti2", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti3", Type.Int) (KNormal.Int 0) $ KNormal.IfEq "Ti2" "Ti3" (KNormal.Int 1) (KNormal.Int 0))
        (KNormal.Int 0)
    specHelper validCase16 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
         KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $ KNormal.Add "x" "Ti0") $
      KNormal.Int 2
    specHelper validCase17 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
         KNormal.Let ("Ti1", Type.Int) (KNormal.Int 1) $ KNormal.Add "x" "Ti1") $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 2) $ KNormal.App "f" ["Ti0"]
    specHelper validCase18 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f", Type.Fun [Type.Int, Type.Int] Type.Int) [("x", Type.Int), ("y", Type.Int)] $
         KNormal.Add "x" "y") $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.App "f" ["Ti0", "Ti1"]
    specHelper validCase19 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("n", Type.Int)] $
         KNormal.Let ("Ti1", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "n"
           "Ti1"
           (KNormal.Int 0)
           (KNormal.Let
              ("Ti4", Type.Int)
              (KNormal.Let ("Ti3", Type.Int) (KNormal.Let ("Ti2", Type.Int) (KNormal.Int 1) $ KNormal.Sub "n" "Ti2") $
               KNormal.App "f" ["Ti3"]) $
            KNormal.Add "n" "Ti4")) $
      KNormal.Let ("Ti0", Type.Int) (KNormal.Int 5) $ KNormal.App "f" ["Ti0"]
    specHelper validCase20 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f", Type.Fun [Type.Int] $ Type.Fun [Type.Int] Type.Int) [("x", Type.Int)] $
         KNormal.LetRec (KNormal.Fundef ("g", Type.Fun [Type.Int] Type.Int) [("y", Type.Int)] $ KNormal.Add "x" "y") $
         KNormal.Var "g") $
      KNormal.Let
        ("Tf1", Type.Fun [Type.Int] Type.Int)
        (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $ KNormal.App "f" ["Ti0"]) $
      KNormal.Let ("Ti2", Type.Int) (KNormal.Int 2) $ KNormal.App "Tf1" ["Ti2"]
    specHelper validCase21 $
      Right $
      KNormal.Let ("x", Type.Int) (KNormal.Int 1) $
      KNormal.LetRec (KNormal.Fundef ("f", Type.Fun [Type.Int] Type.Int) [("y", Type.Int)] $ KNormal.Add "x" "y") $
      KNormal.LetRec
        (KNormal.Fundef ("g", Type.Fun [Type.Int] Type.Int) [("z", Type.Int)] $
         KNormal.Let ("Ti4", Type.Int) (KNormal.Int 2) $ KNormal.Add "z" "Ti4") $
      KNormal.Let
        ("Tf2", Type.Fun [Type.Int] Type.Int)
        (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 3) $
         KNormal.Let ("Ti1", Type.Int) (KNormal.Int 4) $ KNormal.IfEq "Ti0" "Ti1" (KNormal.Var "f") (KNormal.Var "g")) $
      KNormal.Let ("Ti3", Type.Int) (KNormal.Int 5) $ KNormal.App "Tf2" ["Ti3"]
    specHelper validCase22 $
      Right $
      KNormal.Let
        ("Ti4", Type.Int)
        (KNormal.Let
           ("Ti2", Type.Int)
           (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0" "Ti1") $
         KNormal.Let ("Ti3", Type.Int) (KNormal.Int 3) $ KNormal.Add "Ti2" "Ti3") $
      KNormal.Let ("Ti5", Type.Int) (KNormal.Int 4) $ KNormal.Add "Ti4" "Ti5"
    specHelper validCase23 $
      Right $
      KNormal.Let
        ("Ti2", Type.Int)
        (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.Sub "Ti0" "Ti1") $
      KNormal.Let
        ("Ti5", Type.Int)
        (KNormal.Let ("Ti3", Type.Int) (KNormal.Int 3) $
         KNormal.Let ("Ti4", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti3" "Ti4") $
      KNormal.Add "Ti2" "Ti5"
    specHelper validCase24 $
      Right $
      KNormal.Let
        ("Ti4", Type.Int)
        (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
         KNormal.Let
           ("Ti3", Type.Int)
           (KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $
            KNormal.Let ("Ti2", Type.Int) (KNormal.Int 3) $ KNormal.Sub "Ti1" "Ti2") $
         KNormal.Add "Ti0" "Ti3") $
      KNormal.Let ("Ti5", Type.Int) (KNormal.Int 4) $ KNormal.Add "Ti4" "Ti5"
    specHelper validCase25 $
      Right $
      KNormal.Let ("Tv0", Type.Int) (KNormal.Int 42) $
      KNormal.Let ("Tu1", Type.Unit) KNormal.Unit $ KNormal.Let ("Tu2", Type.Unit) KNormal.Unit $ KNormal.Int 1
    specHelper validCase26 $
      Right $
      KNormal.Let
        ("a", Type.Array Type.Int)
        (KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $
         KNormal.Let ("Ti2", Type.Int) (KNormal.Int 1) $ KNormal.ExtFunApp "create_array" ["Ti1", "Ti2"]) $
      KNormal.Let
        ("Tu0", Type.Unit)
        (KNormal.Let ("Ti3", Type.Int) (KNormal.Int 0) $
         KNormal.Let ("Ti4", Type.Int) (KNormal.Int 2) $ KNormal.Put "a" "Ti3" "Ti4") $
      KNormal.Let ("Ti5", Type.Int) (KNormal.Int 1) $ KNormal.Get "a" "Ti5"
    specHelper validCase27 $
      Right $
      KNormal.Let
        ("t", Type.Tuple [Type.Int, Type.Bool, Type.Int])
        (KNormal.Let
           ("Ti2", Type.Int)
           (KNormal.Let ("Ti0", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0" "Ti1") $
         KNormal.Let ("Ti3", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti4", Type.Int) (KNormal.Int 3) $ KNormal.Tuple ["Ti2", "Ti3", "Ti4"]) $
      KNormal.LetTuple [("x", Type.Int), ("b", Type.Bool), ("y", Type.Int)] "t" $
      KNormal.Let ("Ti5", Type.Int) (KNormal.Int 0) $ KNormal.IfEq "b" "Ti5" (KNormal.Sub "x" "y") $ KNormal.Add "x" "y"
  describe "k-normalization without continuations" $ do
    specHelper2 validCase1
    specHelper2 validCase2
    specHelper2 validCase3
    specHelper2 validCase4
    specHelper2 validCase5
    specHelper2 validCase6
    specHelper2 validCase7
    specHelper2 validCase8
    specHelper2 validCase9
    specHelper2 validCase10
    specHelper2 validCase11
    specHelper2 validCase12
    specHelper2 validCase13
    specHelper2 validCase14
    specHelper2 validCase15
    specHelper2 validCase16
    specHelper2 validCase17
    specHelper2 validCase18
    specHelper2 validCase19
    specHelper2 validCase20
    specHelper2 validCase21
    specHelper2 validCase22
    specHelper2 validCase23
    specHelper2 validCase24
    specHelper2 validCase25
    specHelper2 validCase26
