module MinCaml.ElimSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha     as Alpha
import qualified MinCaml.Assoc     as Assoc
import qualified MinCaml.Beta      as Beta
import qualified MinCaml.ConstFold as ConstFold
import qualified MinCaml.Elim      as Elim
import           MinCaml.Global
import qualified MinCaml.Inline    as Inline
import qualified MinCaml.KNormal   as KNormal
import qualified MinCaml.Lexer     as Lexer
import qualified MinCaml.Parser    as Parser
import qualified MinCaml.Type      as Type
import qualified MinCaml.Typing    as Typing

import           Lib               (load, optimize)
import           MinCaml.TestCase

specHelper :: Int -> TestCase -> Either String KNormal.T -> Spec
specHelper numOptimization testCase expected =
  it (name testCase) $
  evalMinCaml (load (input testCase) >>= optimize numOptimization) initialGlobalStatus `shouldBe` expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper 1 validCase1 $ Right KNormal.Unit
    specHelper 1 validCase2 $ Right KNormal.Unit
    specHelper 1 validCase3 $ Right $ KNormal.Int 42
    specHelper 1 validCase4 $ Right $ KNormal.Int 42
    specHelper 1 validCase5 $ Right $ KNormal.Int 3
    specHelper 1 validCase6 $ Right $ KNormal.Int (-1)
    specHelper 1 validCase7 $ Right $ KNormal.Int 0
    specHelper 1 validCase8 $ Right $ KNormal.Int 1
    specHelper 1 validCase9 $ Right $ KNormal.Int 1
    specHelper 1 validCase10 $ Right $ KNormal.Int 0
    specHelper 1 validCase11 $ Right $ KNormal.Int 1
    specHelper 1 validCase12 $ Right $ KNormal.Int 0
    specHelper 1 validCase13 $ Right $ KNormal.Int 42
    specHelper 1 validCase14 $ Right $ KNormal.Int 0
    specHelper 1 validCase15 $ Right $ KNormal.Int 0
    specHelper 1 validCase16 $ Right $ KNormal.Int 2
    specHelper 1 validCase17 $ Right $ KNormal.Int 3
    specHelper 1 validCase18 $ Right $ KNormal.Int 3
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
      KNormal.Let ("Ti3.4.13", Type.Int) (KNormal.Int 4) $
      KNormal.Let ("Ti4.3.14", Type.Int) (KNormal.App "f.0" ["Ti3.4.13"]) $ KNormal.Add "Ti0.6" "Ti4.3.14"
