module MinCaml.InlineSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Assoc    as Assoc
import qualified MinCaml.Beta     as Beta
import           MinCaml.Global
import qualified MinCaml.Inline   as Inline
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
    rest e = Beta.f e >>= Assoc.f >>= Inline.f

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
      KNormal.Let ("Ti0.1", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.0", Type.Int) (KNormal.Neg "Ti0.1") $
      KNormal.Let ("Ti2.4", Type.Int) (KNormal.Int 2) $
      KNormal.Let ("Ti3.3", Type.Int) (KNormal.Neg "Ti2.4") $
      KNormal.Let ("Ti4.5", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti5.2", Type.Int) (KNormal.Sub "Ti3.3" "Ti4.5") $
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
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 2) $
      KNormal.Let ("Ti1.2.4", Type.Int) (KNormal.Int 1) $ KNormal.Add "Ti0.3" "Ti1.2.4"
    specHelper 1 validCase18 $
      Right $
      KNormal.LetRec
        (KNormal.Fundef ("f.0", Type.Fun [Type.Int, Type.Int] Type.Int) [("x.1", Type.Int), ("y.2", Type.Int)] $
         KNormal.Add "x.1" "y.2") $
      KNormal.Let ("Ti0.3", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti1.4", Type.Int) (KNormal.Int 2) $ KNormal.Add "Ti0.3" "Ti1.4"
    specHelper 1 validCase19 $
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
      KNormal.Let ("Ti1.2.11", Type.Int) (KNormal.Int 0) $
      KNormal.IfLe "Ti0.6" "Ti1.2.11" (KNormal.Int 0) $
      KNormal.Let ("Ti2.5.12", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Sub "Ti0.6" "Ti2.5.12") $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.App "f.0" ["Ti3.4.13"]) $ KNormal.Add "Ti0.6" "Ti4.3.14"
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
        (KNormal.Let ("Ti1.2.23", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "Ti3.4.13"
           "Ti1.2.23"
           (KNormal.Int 0)
           (KNormal.Let ("Ti2.5.24", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti3.4.25", Type.Int) (KNormal.Sub "Ti3.4.13" "Ti2.5.24") $
            KNormal.Let ("Ti1.2.7.26", Type.Int) (KNormal.Int 0) $
            KNormal.Let
              ("Ti4.3.27", Type.Int)
              (KNormal.IfLe
                 "Ti3.4.25"
                 "Ti1.2.7.26"
                 (KNormal.Int 0)
                 (KNormal.Let ("Ti2.5.8.28", Type.Int) (KNormal.Int 1) $
                  KNormal.Let ("Ti3.4.9.29", Type.Int) (KNormal.Sub "Ti3.4.25" "Ti2.5.8.28") $
                  KNormal.Let ("Ti4.3.10.30", Type.Int) (KNormal.App "f.0" ["Ti3.4.9.29"]) $
                  KNormal.Add "Ti3.4.25" "Ti4.3.10.30")) $
            KNormal.Add "Ti3.4.13" "Ti4.3.27")) $
      KNormal.Add "Ti0.6" "Ti4.3.14"
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
                  KNormal.Let ("Ti1.2.15", Type.Int) (KNormal.Int 0) $
                  KNormal.Let
                    ("Ti4.3.10", Type.Int)
                    (KNormal.IfLe
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
                              KNormal.Let
                                ("Ti4.3.10.22", Type.Int)
                                (KNormal.Let ("Ti1.2.31", Type.Int) (KNormal.Int 0) $
                                 KNormal.IfLe
                                   "Ti3.4.9.21"
                                   "Ti1.2.31"
                                   (KNormal.Int 0)
                                   (KNormal.Let ("Ti2.5.32", Type.Int) (KNormal.Int 1) $
                                    KNormal.Let ("Ti3.4.33", Type.Int) (KNormal.Sub "Ti3.4.9.21" "Ti2.5.32") $
                                    KNormal.Let ("Ti1.2.7.34", Type.Int) (KNormal.Int 0) $
                                    KNormal.Let
                                      ("Ti4.3.35", Type.Int)
                                      (KNormal.IfLe
                                         "Ti3.4.33"
                                         "Ti1.2.7.34"
                                         (KNormal.Int 0)
                                         (KNormal.Let ("Ti2.5.8.36", Type.Int) (KNormal.Int 1) $
                                          KNormal.Let ("Ti3.4.9.37", Type.Int) (KNormal.Sub "Ti3.4.33" "Ti2.5.8.36") $
                                          KNormal.Let ("Ti1.2.15.38", Type.Int) (KNormal.Int 0) $
                                          KNormal.Let
                                            ("Ti4.3.10.39", Type.Int)
                                            (KNormal.IfLe
                                               "Ti3.4.9.37"
                                               "Ti1.2.15.38"
                                               (KNormal.Int 0)
                                               (KNormal.Let ("Ti2.5.16.40", Type.Int) (KNormal.Int 1) $
                                                KNormal.Let
                                                  ("Ti3.4.17.41", Type.Int)
                                                  (KNormal.Sub "Ti3.4.9.37" "Ti2.5.16.40") $
                                                KNormal.Let ("Ti1.2.7.18.42", Type.Int) (KNormal.Int 0) $
                                                KNormal.Let
                                                  ("Ti4.3.19.43", Type.Int)
                                                  (KNormal.IfLe
                                                     "Ti3.4.17.41"
                                                     "Ti1.2.7.18.42"
                                                     (KNormal.Int 0)
                                                     (KNormal.Let ("Ti2.5.8.20.44", Type.Int) (KNormal.Int 1) $
                                                      KNormal.Let
                                                        ("Ti3.4.9.21.45", Type.Int)
                                                        (KNormal.Sub "Ti3.4.17.41" "Ti2.5.8.20.44") $
                                                      KNormal.Let
                                                        ("Ti4.3.10.22.46", Type.Int)
                                                        (KNormal.App "f.0" ["Ti3.4.9.21.45"]) $
                                                      KNormal.Add "Ti3.4.17.41" "Ti4.3.10.22.46")) $
                                                KNormal.Add "Ti3.4.9.37" "Ti4.3.19.43")) $
                                          KNormal.Add "Ti3.4.33" "Ti4.3.10.39")) $
                                    KNormal.Add "Ti3.4.9.21" "Ti4.3.35")) $
                              KNormal.Add "Ti3.4.17" "Ti4.3.10.22")) $
                        KNormal.Add "Ti3.4.9" "Ti4.3.19")) $
                  KNormal.Add "Ti3.4" "Ti4.3.10")) $
            KNormal.Add "n.1" "Ti4.3")) $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let ("Ti3.4.25", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti3.4.9.29", Type.Int) (KNormal.Int 2) $
      KNormal.Let
        ("Ti4.3.10.30", Type.Int)
        (KNormal.Let ("Ti1.2.47", Type.Int) (KNormal.Int 0) $
         KNormal.IfLe
           "Ti3.4.9.29"
           "Ti1.2.47"
           (KNormal.Int 0)
           (KNormal.Let ("Ti2.5.48", Type.Int) (KNormal.Int 1) $
            KNormal.Let ("Ti3.4.49", Type.Int) (KNormal.Sub "Ti3.4.9.29" "Ti2.5.48") $
            KNormal.Let ("Ti1.2.7.50", Type.Int) (KNormal.Int 0) $
            KNormal.Let
              ("Ti4.3.51", Type.Int)
              (KNormal.IfLe
                 "Ti3.4.49"
                 "Ti1.2.7.50"
                 (KNormal.Int 0)
                 (KNormal.Let ("Ti2.5.8.52", Type.Int) (KNormal.Int 1) $
                  KNormal.Let ("Ti3.4.9.53", Type.Int) (KNormal.Sub "Ti3.4.49" "Ti2.5.8.52") $
                  KNormal.Let ("Ti1.2.15.54", Type.Int) (KNormal.Int 0) $
                  KNormal.Let
                    ("Ti4.3.10.55", Type.Int)
                    (KNormal.IfLe
                       "Ti3.4.9.53"
                       "Ti1.2.15.54"
                       (KNormal.Int 0)
                       (KNormal.Let ("Ti2.5.16.56", Type.Int) (KNormal.Int 1) $
                        KNormal.Let ("Ti3.4.17.57", Type.Int) (KNormal.Sub "Ti3.4.9.53" "Ti2.5.16.56") $
                        KNormal.Let ("Ti1.2.7.18.58", Type.Int) (KNormal.Int 0) $
                        KNormal.Let
                          ("Ti4.3.19.59", Type.Int)
                          (KNormal.IfLe
                             "Ti3.4.17.57"
                             "Ti1.2.7.18.58"
                             (KNormal.Int 0)
                             (KNormal.Let ("Ti2.5.8.20.60", Type.Int) (KNormal.Int 1) $
                              KNormal.Let ("Ti3.4.9.21.61", Type.Int) (KNormal.Sub "Ti3.4.17.57" "Ti2.5.8.20.60") $
                              KNormal.Let ("Ti4.3.10.22.62", Type.Int) (KNormal.App "f.0" ["Ti3.4.9.21.61"]) $
                              KNormal.Add "Ti3.4.17.57" "Ti4.3.10.22.62")) $
                        KNormal.Add "Ti3.4.9.53" "Ti4.3.19.59")) $
                  KNormal.Add "Ti3.4.49" "Ti4.3.10.55")) $
            KNormal.Add "Ti3.4.9.29" "Ti4.3.51")) $
      KNormal.Let ("Ti4.3.27", Type.Int) (KNormal.Add "Ti3.4.25" "Ti4.3.10.30") $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.Add "Ti3.4.13" "Ti4.3.27") $ KNormal.Add "Ti0.6" "Ti4.3.14"
    specHelper 4 validCase19 $
      Right $
      KNormal.Let ("Ti0.6", Type.Int) (KNormal.Int 5) $
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let ("Ti3.4.25", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti3.4.9.29", Type.Int) (KNormal.Int 2) $
      KNormal.Let ("Ti4.3.51", Type.Int) (KNormal.Int 1) $
      KNormal.Let ("Ti4.3.10.30", Type.Int) (KNormal.Add "Ti3.4.9.29" "Ti4.3.51") $
      KNormal.Let ("Ti4.3.27", Type.Int) (KNormal.Add "Ti3.4.25" "Ti4.3.10.30") $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.Add "Ti3.4.13" "Ti4.3.27") $ KNormal.Add "Ti0.6" "Ti4.3.14"
