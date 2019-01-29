module MinCaml.AlphaSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
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
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right KNormal.Unit
    specHelper validCase2 $ Right KNormal.Unit
    specHelper validCase3 $ Right (KNormal.Int 42)
    specHelper validCase4 $ Right (KNormal.Int 42)
    specHelper validCase5 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.0" "Ti1.1"
    specHelper validCase6 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti0.0" "Ti1.1"
    specHelper validCase7 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 6) $ KNormal.IfEq "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase8 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 7) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 8) $ KNormal.IfEq "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase9 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 9) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 10) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase10 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 12) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 11) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase11 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 14) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 13) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase12 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 15) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 16) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper validCase13 $ Right $ KNormal.Let ("x_.0", Type.Int) (KNormal.Int 42) $ KNormal.Var "x_.0"
    specHelper validCase14 $
      Right $
      KNormal.Let ("Ti1.0", Type.Int) (KNormal.Let ("Ti0.1", Type.Int) (KNormal.Int 1) $ KNormal.Neg "Ti0.1") $
      KNormal.Let
        ("Ti5.2", Type.Int)
        (KNormal.Let ("Ti3.3", Type.Int) (KNormal.Let ("Ti2.4", Type.Int) (KNormal.Int 2) $ KNormal.Neg "Ti2.4") $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 3) $ KNormal.Sub "Ti3.3" "Ti4.5") $
      KNormal.IfEq "Ti1.0" "Ti5.2" (KNormal.Int 1) (KNormal.Int 0)
    specHelper validCase15 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 0) $
      KNormal.IfEq
        "Ti0.0"
        "Ti1.1"
        (KNormal.Let ("Ti2.2", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti3.3", Type.Int) (KNormal.Int 0) $ KNormal.IfEq "Ti2.2" "Ti3.3" (KNormal.Int 1) (KNormal.Int 0))
        (KNormal.Int 0)
    specHelper validCase16 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] $
         KNormal.Let ("Ti0.2", Type.Int) (KNormal.Int 1) $ KNormal.Add "x.1" "Ti0.2") $
      KNormal.Int 2
    specHelper validCase17 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 1) $ KNormal.Add "x.1" "Ti1.2") $
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 2) $ KNormal.App "f.0" ["Ti0.3"]
    specHelper validCase18 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int, Type.Int] Type.Int) [("x.1", Type.Int), ("y.2", Type.Int)] $
         KNormal.Add "x.1" "y.2") $
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.4", Type.Int) (KNormal.Int 2) $ KNormal.App "f.0" ["Ti0.3", "Ti1.4"]
    specHelper validCase19 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("n.1", Type.Int)] $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "n.1"
           "Ti1.2"
           (KNormal.Int 0)
           (KNormal.Let
              ("Ti4.3", Type.Int)
              (KNormal.Let
                 ("Ti3.4", Type.Int)
                 (KNormal.Let ("Ti2.5", Type.Int) (KNormal.Int 1) $ KNormal.Sub "n.1" "Ti2.5") $
               KNormal.App "f.0" ["Ti3.4"]) $
            KNormal.Add "n.1" "Ti4.3")) $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $ KNormal.App "f.0" ["Ti0.6"]
    specHelper validCase20 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] $ Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] $
         KNormal.LetRec
           (KNormal.Fundef ("g.2", Type.Fun [Type.Int] Type.Int) [("y.3", Type.Int)] $ KNormal.Add "x.1" "y.3") $
         KNormal.Var "g.2") $
      KNormal.Let
        ("Tf1.4", Type.Fun [Type.Int] Type.Int)
        (KNormal.Let ("Ti0.5", Type.Int) (KNormal.Int 1) $ KNormal.App "f.0" ["Ti0.5"]) $
      KNormal.Let ("Ti2.6", Type.Int) (KNormal.Int 2) $ KNormal.App "Tf1.4" ["Ti2.6"]
    specHelper validCase21 $
      Right $
      KNormal.Let ("x.0", Type.Int) (KNormal.Int 1) $
      KNormal.LetRec
        (KNormal.Fundef ("f.1", Type.Fun [Type.Int] Type.Int) [("y.2", Type.Int)] $ KNormal.Add "x.0" "y.2") $
      KNormal.LetRec
        (KNormal.Fundef ("g.3", Type.Fun [Type.Int] Type.Int) [("z.4", Type.Int)] $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 2) $ KNormal.Add "z.4" "Ti4.5") $
      KNormal.Let
        ("Tf2.6", Type.Fun [Type.Int] Type.Int)
        (KNormal.Let ("Ti0.7", Type.Int) (KNormal.Int 3) $
         KNormal.Let ("Ti1.8", Type.Int) (KNormal.Int 4) $
         KNormal.IfEq "Ti0.7" "Ti1.8" (KNormal.Var "f.1") (KNormal.Var "g.3")) $
      KNormal.Let ("Ti3.9", Type.Int) (KNormal.Int 5) $ KNormal.App "Tf2.6" ["Ti3.9"]
    specHelper validCase22 $
      Right $
      KNormal.Let
        ("Ti4.0", Type.Int)
        (KNormal.Let
           ("Ti2.1", Type.Int)
           (KNormal.Let ("Ti0.2", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti1.3", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.2" "Ti1.3") $
         KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 3) $ KNormal.Add "Ti2.1" "Ti3.4") $
      KNormal.Let ("Ti5.5", Type.Int) (KNormal.Int 4) $ KNormal.Add "Ti4.0" "Ti5.5"
    specHelper validCase23 $
      Right $
      KNormal.Let
        ("Ti2.0", Type.Int)
        (KNormal.Let ("Ti0.1", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 2) $ KNormal.Sub "Ti0.1" "Ti1.2") $
      KNormal.Let
        ("Ti5.3", Type.Int)
        (KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 3) $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti3.4" "Ti4.5") $
      KNormal.Add "Ti2.0" "Ti5.3"
    specHelper validCase24 $
      Right $
      KNormal.Let
        ("Ti4.0", Type.Int)
        (KNormal.Let ("Ti0.1", Type.Int) (KNormal.Int 1) $
         KNormal.Let
           ("Ti3.2", Type.Int)
           (KNormal.Let ("Ti1.3", Type.Int) (KNormal.Int 2) $
            KNormal.Let ("Ti2.4", Type.Int) (KNormal.Int 3) $ KNormal.Sub "Ti1.3" "Ti2.4") $
         KNormal.Add "Ti0.1" "Ti3.2") $
      KNormal.Let ("Ti5.5", Type.Int) (KNormal.Int 4) $ KNormal.Add "Ti4.0" "Ti5.5"
    specHelper validCase25 $
      Right $
      KNormal.Let ("Tv0.0", Type.Int) (KNormal.Int 42) $
      KNormal.Let ("Tu1.1", Type.Unit) KNormal.Unit $ KNormal.Let ("Tu2.2", Type.Unit) KNormal.Unit $ KNormal.Int 1
