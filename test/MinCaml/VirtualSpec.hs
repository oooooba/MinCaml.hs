module MinCaml.VirtualSpec
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
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing
import qualified MinCaml.Virtual  as Virtual

import           MinCaml.TestCase

specHelper :: TestCase -> Either String Asm.Prog -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f >>=
     Virtual.f)
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
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 2) $ Asm.Ans $ Asm.Add "Ti0.0" (Asm.V "Ti1.1")
    specHelper validCase6 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 3) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 4) $ Asm.Ans $ Asm.Sub "Ti0.0" (Asm.V "Ti1.1")
    specHelper validCase7 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 5) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 6) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase8 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 7) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 8) $
      Asm.Ans $ Asm.IfEq "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase9 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 9) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 10) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase10 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 12) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 11) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase11 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 14) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 13) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase12 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 15) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 16) $
      Asm.Ans $ Asm.IfLe "Ti0.0" (Asm.V "Ti1.1") (Asm.Ans $ Asm.Set 0) (Asm.Ans $ Asm.Set 1)
    specHelper validCase13 $ Right $ Asm.Prog [] [] $ Asm.Let ("x_.0", Type.Int) (Asm.Set 42) $ Asm.Ans $ Asm.Mov "x_.0"
    specHelper validCase14 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.1", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti1.0", Type.Int) (Asm.Neg "Ti0.1") $
      Asm.Let ("Ti2.4", Type.Int) (Asm.Set 2) $
      Asm.Let ("Ti3.3", Type.Int) (Asm.Neg "Ti2.4") $
      Asm.Let ("Ti4.5", Type.Int) (Asm.Set 3) $
      Asm.Let ("Ti5.2", Type.Int) (Asm.Sub "Ti3.3" $ Asm.V "Ti4.5") $
      Asm.Ans $ Asm.IfEq "Ti1.0" (Asm.V "Ti5.2") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0)
    specHelper validCase15 $
      Right $
      Asm.Prog [] [] $
      Asm.Let ("Ti0.0", Type.Int) (Asm.Set 1) $
      Asm.Let ("Ti1.1", Type.Int) (Asm.Set 0) $
      Asm.Ans $
      Asm.IfEq
        "Ti0.0"
        (Asm.V "Ti1.1")
        (Asm.Let ("Ti2.2", Type.Int) (Asm.Set 1) $
         Asm.Let ("Ti3.3", Type.Int) (Asm.Set 0) $
         Asm.Ans $ Asm.IfEq "Ti2.2" (Asm.V "Ti3.3") (Asm.Ans $ Asm.Set 1) (Asm.Ans $ Asm.Set 0))
        (Asm.Ans $ Asm.Set 0)
    specHelper validCase16 $
      Right $
      Asm.Prog
        []
        [ Asm.Fundef
            (Id.L "f.0")
            ["x.1"]
            []
            (Asm.Let ("Ti0.2", Type.Int) (Asm.Set 1) $ Asm.Ans $ Asm.Add "x.1" (Asm.V "Ti0.2"))
            Type.Int
        ] $
      Asm.Ans $ Asm.Set 2
    specHelper validCase17 $
      Right $
      Asm.Prog
        []
        [ Asm.Fundef
            (Id.L "f.0")
            ["x.1"]
            []
            (Asm.Let ("Ti1.2", Type.Int) (Asm.Set 1) $ Asm.Ans $ Asm.Add "x.1" (Asm.V "Ti1.2"))
            Type.Int
        ] $
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
            (Asm.Let ("Ti1.2", Type.Int) (Asm.Set 0) $
             Asm.Ans $
             Asm.IfLe "n.1" (Asm.V "Ti1.2") (Asm.Ans $ Asm.Set 0) $
             Asm.Let ("Ti2.5", Type.Int) (Asm.Set 1) $
             Asm.Let ("Ti3.4", Type.Int) (Asm.Sub "n.1" (Asm.V "Ti2.5")) $
             Asm.Let ("Ti4.3", Type.Int) (Asm.CallDir (Id.L "f.0") ["Ti3.4"] []) $
             Asm.Ans $ Asm.Add "n.1" (Asm.V "Ti4.3"))
            Type.Int
        ] $
      Asm.Let ("Ti0.6", Type.Int) (Asm.Set 5) $ Asm.Ans $ Asm.CallDir (Id.L "f.0") ["Ti0.6"] []
