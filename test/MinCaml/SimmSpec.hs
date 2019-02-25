module MinCaml.SimmSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Asm      as Asm
import qualified MinCaml.Closure  as Closure
import           MinCaml.Global
import qualified MinCaml.Id       as Id
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.Simm     as Simm
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing
import qualified MinCaml.Virtual  as Virtual

import           MinCaml.TestCase

specHelper :: TestCase -> Either String Asm.Prog -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f >>=
     Virtual.f >>=
     Simm.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right $ Asm.Prog [] [] (Asm.Ans Asm.Nop)
    specHelper validCase2 $ Right $ Asm.Prog [] [] (Asm.Ans Asm.Nop)
    specHelper validCase3 $ Right $ Asm.Prog [] [] (Asm.Ans $ Asm.Set 42)
    specHelper validCase4 $ Right $ Asm.Prog [] [] (Asm.Ans $ Asm.Set 42)
    specHelper validCase5 $
      Right $ Asm.Prog [] [] $ Asm.Let ("Ti0.0", Type.Int) (Asm.Set 1) $ Asm.Ans $ Asm.Add "Ti0.0" (Asm.C 2)
    specHelper validCase6 $
      Right $ Asm.Prog [] [] $ Asm.Let ("Ti0.0", Type.Int) (Asm.Set 3) $ Asm.Ans $ Asm.Sub "Ti0.0" (Asm.C 4)
    specHelper validCase7 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 5) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.C 6) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase8 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 7) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.C 8) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase9 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 9) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 10) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase10 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 12) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 11) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase11 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 14) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 13) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase12 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 15) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.C 16) (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase13 $ Right $ Asm.Prog [] [] $ Asm.Let ("x_.0", Type.Int) (Asm.Set 42) $ Asm.Ans $ Asm.Mov "x_.0"
    specHelper validCase14 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.1", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti1.0", Type.Int) (Asm.Neg "Ti0.1") $
      Asm.Let ("Ti2.4", Type.Int) (Asm.Set 2) $
      Asm.Let ("Ti3.3", Type.Int) (Asm.Neg "Ti2.4") $
      Asm.Let ("Ti5.2", Type.Int) (Asm.Sub "Ti3.3" $ Asm.C 3) $
      Asm.Ans $ Asm.IfEq "Ti1.0" (Asm.V "Ti5.2") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase15 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 1) $
      Asm.Ans $
      Asm.IfEq
        "Ti0.0"
        (Asm.C 0)
        (Asm.Let ("Ti2.2", Type.Int) (Asm.Set 1) $
         Asm.Ans $ Asm.IfEq "Ti2.2" (Asm.C 0) (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0))
        (Asm.Ans $ Asm.Set 0)
    specHelper validCase16 $
      Right $
      Asm.Prog [] [Asm.Fundef (Id.L "f.0") ["x.1"] [] (Asm.Ans $ Asm.Add "x.1" $ Asm.C 1) Type.Int] $
      Asm.Ans $ Asm.Set 2
    specHelper validCase17 $
      Right $
      Asm.Prog [] [Asm.Fundef (Id.L "f.0") ["x.1"] [] (Asm.Ans $ Asm.Add "x.1" $ Asm.C 1) Type.Int] $
      Asm.Let ("Ti0.3", Type.Int) (Asm.Set 2) $ Asm.Ans $ Asm.CallDir (Id.L "f.0") ["Ti0.3"] []
    specHelper validCase18 $
      Right $
      Asm.Prog [] [Asm.Fundef (Id.L "f.0") ["x.1", "y.2"] [] (Asm.Ans $ Asm.Add "x.1" (Asm.V "y.2")) Type.Int] $
      Asm.Let ("Ti0.3", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti1.4", Type.Int) (Asm.Set 2) $ Asm.Ans $ Asm.CallDir (Id.L "f.0") ["Ti0.3", "Ti1.4"] []
    specHelper validCase19 $
      Right $
      Asm.Prog
        []
        [ Asm.Fundef
            (Id.L "f.0")
            ["n.1"]
            []
            (Asm.Ans $
             Asm.IfLe "n.1" (Asm.C 0) (Asm.Ans $ Asm.Set 0) $
             Asm.Let ("Ti3.4", Type.Int) (Asm.Sub "n.1" (Asm.C 1)) $
             Asm.Let ("Ti4.3", Type.Int) (Asm.CallDir (Id.L "f.0") ["Ti3.4"] []) $
             Asm.Ans $ Asm.Add "n.1" (Asm.V "Ti4.3"))
            Type.Int
        ] $
      Asm.Let ("Ti0.6", Type.Int) (Asm.Set 5) $ Asm.Ans $ Asm.CallDir (Id.L "f.0") ["Ti0.6"] []
    specHelper validCase20 $
      Right $
      Asm.Prog
        []
        [ Asm.Fundef
            (Id.L "g.2")
            ["y.3"]
            []
            (Asm.Let ("x.1", Type.Int) (Asm.Ld "g.2" (Asm.C 8) 1) $ Asm.Ans $ Asm.Add "x.1" (Asm.V "y.3"))
            Type.Int
        , Asm.Fundef
            (Id.L "f.0")
            ["x.1"]
            []
            (Asm.Let ("g.2", Type.Fun [Type.Int] Type.Int) (Asm.Mov Asm.regHp) $
             Asm.Let (Asm.regHp, Type.Int) (Asm.Add Asm.regHp (Asm.C 16)) $
             Asm.Let ("l.7", Type.Int) (Asm.SetL $ Id.L "g.2") $
             Asm.Let ("Tu4", Type.Unit) (Asm.St "l.7" "g.2" (Asm.C 0) 1) $
             Asm.Let ("Tu3", Type.Unit) (Asm.St "x.1" "g.2" (Asm.C 8) 1) $ Asm.Ans $ Asm.Mov "g.2") $
          Type.Fun [Type.Int] Type.Int
        ] $
      Asm.Let ("Ti0.5", Type.Int) (Asm.Set 1) $
      Asm.Let ("Tf1.4", Type.Fun [Type.Int] Type.Int) (Asm.CallDir (Id.L "f.0") ["Ti0.5"] []) $
      Asm.Let ("Ti2.6", Type.Int) (Asm.Set 2) $ Asm.Ans $ Asm.CallCls "Tf1.4" ["Ti2.6"] []
    specHelper validCase21 $
      Right $
      Asm.Prog
        []
        [ Asm.Fundef
            (Id.L "f.1")
            ["y.2"]
            []
            (Asm.Let ("x.0", Type.Int) (Asm.Ld "f.1" (Asm.C 8) 1) $ Asm.Ans $ Asm.Add "x.0" $ Asm.V "y.2")
            Type.Int
        , Asm.Fundef (Id.L "g.3") ["z.4"] [] (Asm.Ans $ Asm.Add "z.4" $ Asm.C 2) Type.Int
        ] $
      Asm.Let ("x.0", Type.Int) (Asm.Set 1) $
      Asm.Let ("f.1", Type.Fun [Type.Int] Type.Int) (Asm.Mov Asm.regHp) $
      Asm.Let (Asm.regHp, Type.Int) (Asm.Add Asm.regHp (Asm.C 16)) $
      Asm.Let ("l.11", Type.Int) (Asm.SetL $ Id.L "f.1") $
      Asm.Let ("Tu7", Type.Unit) (Asm.St "l.11" "f.1" (Asm.C 0) 1) $
      Asm.Let ("Tu6", Type.Unit) (Asm.St "x.0" "f.1" (Asm.C 8) 1) $
      Asm.Let ("g.3", Type.Fun [Type.Int] Type.Int) (Asm.Mov Asm.regHp) $
      Asm.Let (Asm.regHp, Type.Int) (Asm.Add Asm.regHp (Asm.C 8)) $
      Asm.Let ("l.10", Type.Int) (Asm.SetL $ Id.L "g.3") $
      Asm.Let ("Tu5", Type.Unit) (Asm.St "l.10" "g.3" (Asm.C 0) 1) $
      Asm.Let ("Ti0.7", Type.Int) (Asm.Set 3) $
      Asm.Let
        ("Tf2.6", Type.Fun [Type.Int] Type.Int)
        (Asm.IfEq "Ti0.7" (Asm.C 4) (Asm.Ans $ Asm.Mov "f.1") (Asm.Ans $ Asm.Mov "g.3")) $
      Asm.Let ("Ti3.9", Type.Int) (Asm.Set 5) $ Asm.Ans $ Asm.CallCls "Tf2.6" ["Ti3.9"] []
    specHelper validCase22 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.2", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti2.1", Type.Int) (Asm.Add "Ti0.2" (Asm.C 2)) $
      Asm.Let ("Ti4.0", Type.Int) (Asm.Add "Ti2.1" (Asm.C 3)) $ Asm.Ans $ Asm.Add "Ti4.0" (Asm.C 4)
    specHelper validCase23 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.1", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti2.0", Type.Int) (Asm.Sub "Ti0.1" $ Asm.C 2) $
      Asm.Let ("Ti3.4", Type.Int) (Asm.Set 3) $
      Asm.Let ("Ti5.3", Type.Int) (Asm.Sub "Ti3.4" $ Asm.C 4) $ Asm.Ans $ Asm.Add "Ti2.0" $ Asm.V "Ti5.3"
    specHelper validCase24 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti1.3", Type.Int) (Asm.Set 2) $
      Asm.Let ("Ti3.2", Type.Int) (Asm.Sub "Ti1.3" $ Asm.C 3) $
      Asm.Let ("Ti4.0", Type.Int) (Asm.Add "Ti3.2" $ Asm.C 1) $ Asm.Ans $ Asm.Add "Ti4.0" $ Asm.C 4
    specHelper validCase25 $
      Right $
      Asm.Prog [] [] $ Asm.Let ("Tu1.1", Type.Unit) Asm.Nop $ Asm.Let ("Tu2.2", Type.Unit) Asm.Nop $ Asm.Ans $ Asm.Set 1
    specHelper validCase26 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 2) $
      Asm.Let ("Ti2.2", Type.Int) (Asm.Set 1) $
      Asm.Let ("a.0", Type.Array Type.Int) (Asm.CallDir (Id.L "min_caml_create_array") ["Ti1.1", "Ti2.2"] []) $
      Asm.Let ("Ti4.5", Type.Int) (Asm.Set 2) $
      Asm.Let ("Tu0.3", Type.Unit) (Asm.St "Ti4.5" "a.0" (Asm.C 0) 8) $ Asm.Ans $ Asm.Ld "a.0" (Asm.C 1) 8
    specHelper validCase27 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.2", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti2.1", Type.Int) (Asm.Add "Ti0.2" $ Asm.C 2) $
      Asm.Let ("Ti3.4", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti4.5", Type.Int) (Asm.Set 3) $
      Asm.Let ("t.10", Type.Tuple [Type.Int, Type.Int, Type.Int]) (Asm.Mov Asm.regHp) $
      Asm.Let (Asm.regHp, Type.Int) (Asm.Add Asm.regHp $ Asm.C 24) $
      Asm.Let ("Tu8", Type.Unit) (Asm.St "Ti4.5" "t.10" (Asm.C 16) 1) $
      Asm.Let ("Tu7", Type.Unit) (Asm.St "Ti3.4" "t.10" (Asm.C 8) 1) $
      Asm.Let ("Tu6", Type.Unit) (Asm.St "Ti2.1" "t.10" (Asm.C 0) 1) $
      Asm.Let ("t.0", Type.Tuple [Type.Int, Type.Bool, Type.Int]) (Asm.Mov "t.10") $
      Asm.Let ("y.8", Type.Int) (Asm.Ld "t.0" (Asm.C 16) 1) $
      Asm.Let ("b.7", Type.Bool) (Asm.Ld "t.0" (Asm.C 8) 1) $
      Asm.Let ("x.6", Type.Int) (Asm.Ld "t.0" (Asm.C 0) 1) $
      Asm.Ans $ Asm.IfEq "b.7" (Asm.C 0) (Asm.Ans $ Asm.Sub "x.6" $ Asm.V "y.8") $ Asm.Ans $ Asm.Add "x.6" $ Asm.V "y.8"
