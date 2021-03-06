module MinCaml.BetaSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Beta     as Beta
import           MinCaml.Global
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           Lib              (load, optimize)
import           MinCaml.TestCase

specHelper :: Int -> TestCase -> Either String KNormal.T -> Spec
specHelper numOptimization testCase expected =
  it (name testCase) $
  evalMinCaml (load (input testCase) >>= optimize (numOptimization - 1) >>= rest) initialGlobalStatus `shouldBe`
  expected
  where
    rest = Beta.f

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper 1 validCase1 $ Right KNormal.Unit
    specHelper 1 validCase2 $ Right KNormal.Unit
    specHelper 1 validCase3 $ Right $ KNormal.Int 42
    specHelper 1 validCase4 $ Right $ KNormal.Int 42
    specHelper 1 validCase5 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.0" "Ti1.1"
    specHelper 1 validCase6 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 4) $ KNormal.Sub "Ti0.0" "Ti1.1"
    specHelper 1 validCase7 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 6) $ KNormal.IfEq "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper 1 validCase8 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 7) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 8) $ KNormal.IfEq "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper 1 validCase9 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 9) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 10) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper 1 validCase10 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 12) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 11) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 1) (KNormal.Int 0)
    specHelper 1 validCase11 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 14) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 13) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper 1 validCase12 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 15) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 16) $ KNormal.IfLe "Ti0.0" "Ti1.1" (KNormal.Int 0) (KNormal.Int 1)
    specHelper 1 validCase13 $ Right $ KNormal.Let ("x_.0", Type.Int) (KNormal.Int 42) $ KNormal.Var "x_.0"
    specHelper 1 validCase14 $
      Right $
      KNormal.Let ("Ti1.0", Type.Int) (KNormal.Let ("Ti0.1", Type.Int) (KNormal.Int 1) $ KNormal.Neg "Ti0.1") $
      KNormal.Let
        ("Ti5.2", Type.Int)
        (KNormal.Let ("Ti3.3", Type.Int) (KNormal.Let ("Ti2.4", Type.Int) (KNormal.Int 2) $ KNormal.Neg "Ti2.4") $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 3) $ KNormal.Sub "Ti3.3" "Ti4.5") $
      KNormal.IfEq "Ti1.0" "Ti5.2" (KNormal.Int 1) (KNormal.Int 0)
    specHelper 1 validCase15 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 0) $
      KNormal.IfEq
        "Ti0.0"
        "Ti1.1"
        (KNormal.Let ("Ti2.2", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti3.3", Type.Int) (KNormal.Int 0) $ KNormal.IfEq "Ti2.2" "Ti3.3" (KNormal.Int 1) (KNormal.Int 0))
        (KNormal.Int 0)
    specHelper 1 validCase16 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] $
         KNormal.Let ("Ti0.2", Type.Int) (KNormal.Int 1) $ KNormal.Add "x.1" "Ti0.2") $
      KNormal.Int 2
    specHelper 1 validCase17 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 1) $ KNormal.Add "x.1" "Ti1.2") $
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 2) $ KNormal.App "f.0" ["Ti0.3"]
    specHelper 1 validCase18 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int, Type.Int] Type.Int) [("x.1", Type.Int), ("y.2", Type.Int)] $
         KNormal.Add "x.1" "y.2") $
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.4", Type.Int) (KNormal.Int 2) $ KNormal.App "f.0" ["Ti0.3", "Ti1.4"]
    specHelper 1 validCase19 $
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
    specHelper 2 validCase19 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("n.1", Type.Int)] $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "n.1"
           "Ti1.2"
           (KNormal.Int 0)
           (KNormal.Let ("Ti2.5", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti3.4", Type.Int) (KNormal.Sub "n.1" "Ti2.5") $
            KNormal.Let
              ("Ti4.3", Type.Int)
              (KNormal.Let ("Ti1.2.7", Type.Int) (KNormal.Int 0) $
               KNormal.IfLe
                 "Ti3.4"
                 "Ti1.2.7"
                 (KNormal.Int 0)
                 (KNormal.Let ("Ti2.5.8", Type.Int) (KNormal.Int 1) $
                  KNormal.Let ("Ti3.4.9", Type.Int) (KNormal.Sub "Ti3.4" "Ti2.5.8") $
                  KNormal.Let ("Ti4.3.10", Type.Int) (KNormal.App "f.0" ["Ti3.4.9"]) $ KNormal.Add "Ti3.4" "Ti4.3.10")) $
            KNormal.Add "n.1" "Ti4.3")) $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.App "f.0" ["Ti3.4.13"]) $ KNormal.Add "Ti0.6" "Ti4.3.14"
    specHelper 3 validCase19 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int] Type.Int) [("n.1", Type.Int)] $
         KNormal.Let ("Ti1.2", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "n.1"
           "Ti1.2"
           (KNormal.Int 0)
           (KNormal.Let ("Ti2.5", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti3.4", Type.Int) (KNormal.Sub "n.1" "Ti2.5") $
            KNormal.Let ("Ti1.2.7", Type.Int) (KNormal.Int 0) $
            KNormal.Let
              ("Ti4.3", Type.Int)
              (KNormal.IfLe
                 "Ti3.4"
                 "Ti1.2.7"
                 (KNormal.Int 0)
                 (KNormal.Let ("Ti2.5.8", Type.Int) (KNormal.Int 1) $
                  KNormal.Let ("Ti3.4.9", Type.Int) (KNormal.Sub "Ti3.4" "Ti2.5.8") $
                  KNormal.Let
                    ("Ti4.3.10", Type.Int)
                    (KNormal.Let ("Ti1.2.15", Type.Int) (KNormal.Int 0) $
                     KNormal.IfLe
                       "Ti3.4.9"
                       "Ti1.2.15"
                       (KNormal.Int 0)
                       (KNormal.Let ("Ti2.5.16", Type.Int) (KNormal.Int 1) $
                        KNormal.Let ("Ti3.4.17", Type.Int) (KNormal.Sub "Ti3.4.9" "Ti2.5.16") $
                        KNormal.Let ("Ti1.2.7.18", Type.Int) (KNormal.Int 0) $
                        KNormal.Let
                          ("Ti4.3.19", Type.Int)
                          (KNormal.IfLe
                             "Ti3.4.17"
                             "Ti1.2.7.18"
                             (KNormal.Int 0)
                             (KNormal.Let ("Ti2.5.8.20", Type.Int) (KNormal.Int 1) $
                              KNormal.Let ("Ti3.4.9.21", Type.Int) (KNormal.Sub "Ti3.4.17" "Ti2.5.8.20") $
                              KNormal.Let ("Ti4.3.10.22", Type.Int) (KNormal.App "f.0" ["Ti3.4.9.21"]) $
                              KNormal.Add "Ti3.4.17" "Ti4.3.10.22")) $
                        KNormal.Add "Ti3.4.9" "Ti4.3.19")) $
                  KNormal.Add "Ti3.4" "Ti4.3.10")) $
            KNormal.Add "n.1" "Ti4.3")) $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let
        ("Ti4.3.14", Type.Int)
        (KNormal.Let ("Ti3.4.25", Type.Int) (KNormal.Int 3) $
         KNormal.Let
           ("Ti4.3.27", Type.Int)
           (KNormal.Let ("Ti3.4.9.29", Type.Int) (KNormal.Int 2) $
            KNormal.Let ("Ti4.3.10.30", Type.Int) (KNormal.App "f.0" ["Ti3.4.9.29"]) $
            KNormal.Add "Ti3.4.25" "Ti4.3.10.30") $
         KNormal.Add "Ti3.4.13" "Ti4.3.27") $
      KNormal.Add "Ti0.6" "Ti4.3.14"
    specHelper 4 validCase19 $
      Right $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let ("Ti3.4.25", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti3.4.9.29", Type.Int) (KNormal.Int 2) $
      KNormal.Let
        ("Ti4.3.10.30", Type.Int)
        (KNormal.Let ("Ti4.3.51", Type.Int) (KNormal.Int 1) $ KNormal.Add "Ti3.4.9.29" "Ti4.3.51") $
      KNormal.Let ("Ti4.3.27", Type.Int) (KNormal.Add "Ti3.4.25" "Ti4.3.10.30") $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.Add "Ti3.4.13" "Ti4.3.27") $ KNormal.Add "Ti0.6" "Ti4.3.14"
    specHelper 1 validCase20 $
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
    specHelper 2 validCase20 $
      Right $
      KNormal.Let ("Ti0.5", Type.Int) (KNormal.Int 1) $
      KNormal.Let
        ("Tf1.4", Type.Fun [Type.Int] Type.Int)
        (KNormal.LetRec
           (KNormal.Fundef ("g.2.7", Type.Fun [Type.Int] Type.Int) [("y.3.8", Type.Int)] $ KNormal.Add "Ti0.5" "y.3.8") $
         KNormal.Var "g.2.7") $
      KNormal.Let ("Ti2.6", Type.Int) (KNormal.Int 2) $ KNormal.App "Tf1.4" ["Ti2.6"]
    specHelper 3 validCase20 $
      Right $
      KNormal.Let ("Ti0.5", Type.Int) (KNormal.Int 1) $
      KNormal.LetRec
        (KNormal.Fundef ("g.2.7", Type.Fun [Type.Int] Type.Int) [("y.3.8", Type.Int)] $ KNormal.Add "Ti0.5" "y.3.8") $
      KNormal.Let ("Ti2.6", Type.Int) (KNormal.Int 2) $ KNormal.App "g.2.7" ["Ti2.6"]
    specHelper 1 validCase21 $
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
    specHelper 2 validCase21 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("g.3", Type.Fun [Type.Int] Type.Int) [("z.4", Type.Int)] $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 2) $ KNormal.Add "z.4" "Ti4.5") $
      KNormal.Let ("Ti3.9", Type.Int) (KNormal.Int 5) $ KNormal.App "g.3" ["Ti3.9"]
    specHelper 1 validCase22 $
      Right $
      KNormal.Let
        ("Ti4.0", Type.Int)
        (KNormal.Let
           ("Ti2.1", Type.Int)
           (KNormal.Let ("Ti0.2", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti1.3", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.2" "Ti1.3") $
         KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 3) $ KNormal.Add "Ti2.1" "Ti3.4") $
      KNormal.Let ("Ti5.5", Type.Int) (KNormal.Int 4) $ KNormal.Add "Ti4.0" "Ti5.5"
    specHelper 1 validCase23 $
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
    specHelper 1 validCase24 $
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
    specHelper 1 validCase25 $
      Right $
      KNormal.Let ("Tv0.0", Type.Int) (KNormal.Int 42) $
      KNormal.Let ("Tu1.1", Type.Unit) KNormal.Unit $ KNormal.Let ("Tu2.2", Type.Unit) KNormal.Unit $ KNormal.Int 1
    specHelper 1 validCase26 $
      Right $
      KNormal.Let
        ("a.0", Type.Array Type.Int)
        (KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 2) $
         KNormal.Let ("Ti2.2", Type.Int) (KNormal.Int 1) $ KNormal.ExtFunApp "create_array" ["Ti1.1", "Ti2.2"]) $
      KNormal.Let
        ("Tu0.3", Type.Unit)
        (KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 0) $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 2) $ KNormal.Put "a.0" "Ti3.4" "Ti4.5") $
      KNormal.Let ("Ti5.6", Type.Int) (KNormal.Int 1) $ KNormal.Get "a.0" "Ti5.6"
    specHelper 1 validCase27 $
      Right $
      KNormal.Let
        ("t.0", Type.Tuple [Type.Int, Type.Bool, Type.Int])
        (KNormal.Let
           ("Ti2.1", Type.Int)
           (KNormal.Let ("Ti0.2", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti1.3", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.2" "Ti1.3") $
         KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 1) $
         KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 3) $ KNormal.Tuple ["Ti2.1", "Ti3.4", "Ti4.5"]) $
      KNormal.LetTuple [("x.6", Type.Int), ("b.7", Type.Bool), ("y.8", Type.Int)] "t.0" $
      KNormal.Let ("Ti5.9", Type.Int) (KNormal.Int 0) $
      KNormal.IfEq "b.7" "Ti5.9" (KNormal.Sub "x.6" "y.8") $ KNormal.Add "x.6" "y.8"
    specHelper 2 validCase27 $
      Right $
      KNormal.Let ("Ti2.1", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti3.4", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti5.9", Type.Int) (KNormal.Int 0) $
      KNormal.IfEq "Ti3.4" "Ti5.9" (KNormal.Sub "Ti2.1" "Ti4.5") $ KNormal.Add "Ti2.1" "Ti4.5"
