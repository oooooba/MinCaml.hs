module MinCaml.ClosureSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Closure  as Closure
import           MinCaml.Global
import qualified MinCaml.Id       as Id
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String Closure.Prog -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right $ Closure.Prog [] Closure.Unit
    specHelper validCase2 $ Right $ Closure.Prog [] Closure.Unit
    specHelper validCase3 $ Right $ Closure.Prog [] (Closure.Int 42)
    specHelper validCase4 $ Right $ Closure.Prog [] (Closure.Int 42)
    specHelper validCase5 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 1) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 2) $ Closure.Add "Ti0.0" "Ti1.1"
    specHelper validCase6 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 3) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 4) $ Closure.Sub "Ti0.0" "Ti1.1"
    specHelper validCase7 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 5) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 6) $ Closure.IfEq "Ti0.0" "Ti1.1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase8 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 7) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 8) $ Closure.IfEq "Ti0.0" "Ti1.1" (Closure.Int 0) (Closure.Int 1)
    specHelper validCase9 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 9) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 10) $ Closure.IfLe "Ti0.0" "Ti1.1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase10 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 12) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 11) $ Closure.IfLe "Ti0.0" "Ti1.1" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase11 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 14) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 13) $ Closure.IfLe "Ti0.0" "Ti1.1" (Closure.Int 0) (Closure.Int 1)
    specHelper validCase12 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 15) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 16) $ Closure.IfLe "Ti0.0" "Ti1.1" (Closure.Int 0) (Closure.Int 1)
    specHelper validCase13 $
      Right $ Closure.Prog [] $ Closure.Let ("x_.0", Type.Int) (Closure.Int 42) $ Closure.Var "x_.0"
    specHelper validCase14 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti1.0", Type.Int) (Closure.Let ("Ti0.1", Type.Int) (Closure.Int 1) $ Closure.Neg "Ti0.1") $
      Closure.Let
        ("Ti5.2", Type.Int)
        (Closure.Let ("Ti3.3", Type.Int) (Closure.Let ("Ti2.4", Type.Int) (Closure.Int 2) $ Closure.Neg "Ti2.4") $
         Closure.Let ("Ti4.5", Type.Int) (Closure.Int 3) $ Closure.Sub "Ti3.3" "Ti4.5") $
      Closure.IfEq "Ti1.0" "Ti5.2" (Closure.Int 1) (Closure.Int 0)
    specHelper validCase15 $
      Right $
      Closure.Prog [] $
      Closure.Let ("Ti0.0", Type.Int) (Closure.Int 1) $
      Closure.Let ("Ti1.1", Type.Int) (Closure.Int 0) $
      Closure.IfEq
        "Ti0.0"
        "Ti1.1"
        (Closure.Let ("Ti2.2", Type.Int) (Closure.Int 1) $
         Closure.Let ("Ti3.3", Type.Int) (Closure.Int 0) $ Closure.IfEq "Ti2.2" "Ti3.3" (Closure.Int 1) (Closure.Int 0))
        (Closure.Int 0)
    specHelper validCase16 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] [] $
          Closure.Let ("Ti0.2", Type.Int) (Closure.Int 1) $ Closure.Add "x.1" "Ti0.2"
        ] $
      Closure.Int 2
    specHelper validCase17 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "f.0", Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] [] $
          Closure.Let ("Ti1.2", Type.Int) (Closure.Int 1) $ Closure.Add "x.1" "Ti1.2"
        ] $
      Closure.Let ("Ti0.3", Type.Int) (Closure.Int 2) $ Closure.AppDir (Id.L "f.0") ["Ti0.3"]
    specHelper validCase18 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "f.0", Type.Fun [Type.Int, Type.Int] Type.Int) [("x.1", Type.Int), ("y.2", Type.Int)] [] $
          Closure.Add "x.1" "y.2"
        ] $
      Closure.Let ("Ti0.3", Type.Int) (Closure.Int 1) $
      Closure.Let ("Ti1.4", Type.Int) (Closure.Int 2) $ Closure.AppDir (Id.L "f.0") ["Ti0.3", "Ti1.4"]
    specHelper validCase19 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "f.0", Type.Fun [Type.Int] Type.Int) [("n.1", Type.Int)] [] $
          Closure.Let ("Ti1.2", Type.Int) (Closure.Int 0) $
          Closure.IfLe
            "n.1"
            "Ti1.2"
            (Closure.Int 0)
            (Closure.Let
               ("Ti4.3", Type.Int)
               (Closure.Let
                  ("Ti3.4", Type.Int)
                  (Closure.Let ("Ti2.5", Type.Int) (Closure.Int 1) $ Closure.Sub "n.1" "Ti2.5") $
                Closure.AppDir (Id.L "f.0") ["Ti3.4"]) $
             Closure.Add "n.1" "Ti4.3")
        ] $
      Closure.Let ("Ti0.6", Type.Int) (Closure.Int 5) $ Closure.AppDir (Id.L "f.0") ["Ti0.6"]
    specHelper validCase20 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "g.2", Type.Fun [Type.Int] Type.Int) [("y.3", Type.Int)] [("x.1", Type.Int)] $
          Closure.Add "x.1" "y.3"
        , Closure.Fundef (Id.L "f.0", Type.Fun [Type.Int] $ Type.Fun [Type.Int] Type.Int) [("x.1", Type.Int)] [] $
          Closure.MakeCls ("g.2", Type.Fun [Type.Int] Type.Int) (Closure.Closure (Id.L "g.2") ["x.1"]) $
          Closure.Var "g.2"
        ] $
      Closure.Let
        ("Tf1.4", Type.Fun [Type.Int] Type.Int)
        (Closure.Let ("Ti0.5", Type.Int) (Closure.Int 1) $ Closure.AppDir (Id.L "f.0") ["Ti0.5"]) $
      Closure.Let ("Ti2.6", Type.Int) (Closure.Int 2) $ Closure.AppCls "Tf1.4" ["Ti2.6"]
    specHelper validCase21 $
      Right $
      Closure.Prog
        [ Closure.Fundef (Id.L "f.1", Type.Fun [Type.Int] Type.Int) [("y.2", Type.Int)] [("x.0", Type.Int)] $
          Closure.Add "x.0" "y.2"
        --
        , Closure.Fundef (Id.L "g.3", Type.Fun [Type.Int] Type.Int) [("z.4", Type.Int)] [] $
          Closure.Let ("Ti4.5", Type.Int) (Closure.Int 2) $ Closure.Add "z.4" "Ti4.5"
        ] $
      Closure.Let ("x.0", Type.Int) (Closure.Int 1) $
      Closure.MakeCls ("f.1", Type.Fun [Type.Int] Type.Int) (Closure.Closure (Id.L "f.1") ["x.0"]) $
      Closure.MakeCls ("g.3", Type.Fun [Type.Int] Type.Int) (Closure.Closure (Id.L "g.3") []) $
      Closure.Let
        ("Tf2.6", Type.Fun [Type.Int] Type.Int)
        (Closure.Let ("Ti0.7", Type.Int) (Closure.Int 3) $
         Closure.Let ("Ti1.8", Type.Int) (Closure.Int 4) $
         Closure.IfEq "Ti0.7" "Ti1.8" (Closure.Var "f.1") (Closure.Var "g.3")) $
      Closure.Let ("Ti3.9", Type.Int) (Closure.Int 5) $ Closure.AppCls "Tf2.6" ["Ti3.9"]
