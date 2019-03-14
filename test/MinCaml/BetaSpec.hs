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
