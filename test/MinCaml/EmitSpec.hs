module MinCaml.EmitSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha    as Alpha
import qualified MinCaml.Asm      as Asm
import qualified MinCaml.Closure  as Closure
import qualified MinCaml.Emit     as Emit
import           MinCaml.Global
import qualified MinCaml.Id       as Id
import qualified MinCaml.KNormal  as KNormal
import qualified MinCaml.Lexer    as Lexer
import qualified MinCaml.Parser   as Parser
import qualified MinCaml.RegAlloc as RegAlloc
import qualified MinCaml.Simm     as Simm
import qualified MinCaml.Type     as Type
import qualified MinCaml.Typing   as Typing
import qualified MinCaml.Virtual  as Virtual

import           MinCaml.TestCase

specHelper :: TestCase -> Either String ([String], [String], [String]) -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f >>=
     Virtual.f >>=
     Simm.f >>=
     RegAlloc.f >>=
     Emit.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right ([], [], [])
    specHelper validCase2 $ Right ([], [], [])
    specHelper validCase3 $ Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 42])
    specHelper validCase4 $ Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 42])
    specHelper validCase5 $
      Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1, Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 2])
    specHelper validCase6 $
      Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 3, Asm.instrSub (Asm.Reg Asm.regRax) $ Asm.Imm 4])
    specHelper validCase7 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 5
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 6
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.3"
          , Asm.pinstrLabel "ifeq_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifeq_nontail_cont.3"
          ])
    specHelper validCase8 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 7
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 8
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.3"
          , Asm.pinstrLabel "ifeq_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.pinstrLabel "ifeq_nontail_cont.3"
          ])
    specHelper validCase9 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 9
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 10
          , Asm.instrJg $ Asm.Lab "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrJmp $ Asm.Lab "ifle_nontail_cont.3"
          , Asm.pinstrLabel "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifle_nontail_cont.3"
          ])
    specHelper validCase10 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 12
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 11
          , Asm.instrJg $ Asm.Lab "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrJmp $ Asm.Lab "ifle_nontail_cont.3"
          , Asm.pinstrLabel "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifle_nontail_cont.3"
          ])
    specHelper validCase11 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 14
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 13
          , Asm.instrJg $ Asm.Lab "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrJmp $ Asm.Lab "ifle_nontail_cont.3"
          , Asm.pinstrLabel "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.pinstrLabel "ifle_nontail_cont.3"
          ])
    specHelper validCase12 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 15
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 16
          , Asm.instrJg $ Asm.Lab "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrJmp $ Asm.Lab "ifle_nontail_cont.3"
          , Asm.pinstrLabel "ifle_nontail_else.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.pinstrLabel "ifle_nontail_cont.3"
          ])
    specHelper validCase13 $ Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 42])
    specHelper validCase14 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrNeg $ Asm.Reg Asm.regRax
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 2
          , Asm.instrNeg $ Asm.Reg Asm.regRdi
          , Asm.instrSub (Asm.Reg Asm.regRdi) $ Asm.Imm 3
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.6"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.7"
          , Asm.pinstrLabel "ifeq_nontail_else.6"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifeq_nontail_cont.7"
          ])
    specHelper validCase15 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.4"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrCmp (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.6"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.7"
          , Asm.pinstrLabel "ifeq_nontail_else.6"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifeq_nontail_cont.7"
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.5"
          , Asm.pinstrLabel "ifeq_nontail_else.4"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.pinstrLabel "ifeq_nontail_cont.5"
          ])
    specHelper validCase16 $
      Right
        ( []
        , [ Asm.pinstrLabel "f.0"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrRet
          ]
        , [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 2])
    specHelper validCase17 $
      Right
        ( []
        , [ Asm.pinstrLabel "f.0"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrRet
          ]
        , [Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 2, Asm.instrCall (Asm.Lab "f.0")])
    specHelper validCase18 $
      Right
        ( []
        , [ Asm.pinstrLabel "f.0"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRsi
          , Asm.instrRet
          ]
        , [ Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 1
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Imm 2
          , Asm.instrCall (Asm.Lab "f.0")
          ])
    specHelper validCase19 $
      Right
        ( []
        , [ Asm.pinstrLabel "f.0"
          , Asm.instrCmp (Asm.Reg Asm.regRdi) $ Asm.Imm 0
          , Asm.instrJg (Asm.Lab "ifle_tail_else.7")
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 0
          , Asm.instrRet
          , Asm.pinstrLabel "ifle_tail_else.7"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrSub (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrMov (Asm.Mem Asm.regSp 0) $ Asm.Reg Asm.regRdi
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Reg Asm.regRax
          , Asm.instrAdd (Asm.Reg Asm.regSp) $ Asm.Imm 8
          , Asm.instrCall (Asm.Lab "f.0")
          , Asm.instrSub (Asm.Reg Asm.regSp) $ Asm.Imm 8
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Mem Asm.regSp 0
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrRet
          ]
        , [Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 5, Asm.instrCall (Asm.Lab "f.0")])
    specHelper validCase20 $
      Right
        ( []
        , [ Asm.pinstrLabel "g.2"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Mem Asm.regR8 8
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrRet
          --------------------------------------------------------------------------------------------------------
          , Asm.pinstrLabel "f.0"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regHp
          , Asm.instrAdd (Asm.Reg Asm.regHp) $ Asm.Imm 16
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Lab "g.2"
          , Asm.instrMov (Asm.Mem Asm.regRax 0) $ Asm.Reg Asm.regRsi
          , Asm.instrMov (Asm.Mem Asm.regRax 8) $ Asm.Reg Asm.regRdi
          , Asm.instrRet
          ]
        , [ Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 1
          , Asm.instrCall $ Asm.Lab "f.0"
          , Asm.instrMov (Asm.Reg Asm.regR8) $ Asm.Reg Asm.regRax
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 2
          , Asm.instrCall $ Asm.Reg Asm.regR8
          ])
    specHelper validCase21 $
      Right
        ( []
        , [ Asm.pinstrLabel "f.1"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Mem Asm.regR8 8
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrRet
          --------------------------------------------------------------------------------------------------------
          , Asm.pinstrLabel "g.3"
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 2
          , Asm.instrRet
          ]
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Reg Asm.regHp
          , Asm.instrAdd (Asm.Reg Asm.regHp) $ Asm.Imm 16
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Lab "f.1"
          , Asm.instrMov (Asm.Mem Asm.regRdi 0) $ Asm.Reg Asm.regRsi
          , Asm.instrMov (Asm.Mem Asm.regRdi 8) $ Asm.Reg Asm.regRax
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regHp
          , Asm.instrAdd (Asm.Reg Asm.regHp) $ Asm.Imm 8
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Lab "g.3"
          , Asm.instrMov (Asm.Mem Asm.regRax 0) $ Asm.Reg Asm.regRsi
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Imm 3
          , Asm.instrCmp (Asm.Reg Asm.regRsi) $ Asm.Imm 4
          , Asm.instrJne $ Asm.Lab "ifeq_nontail_else.12"
          , Asm.instrMov (Asm.Reg Asm.regR8) $ Asm.Reg Asm.regRdi
          , Asm.instrJmp $ Asm.Lab "ifeq_nontail_cont.13"
          , Asm.pinstrLabel "ifeq_nontail_else.12"
          , Asm.instrMov (Asm.Reg Asm.regR8) $ Asm.Reg Asm.regRax
          , Asm.pinstrLabel "ifeq_nontail_cont.13"
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 5
          , Asm.instrCall $ Asm.Reg Asm.regR8
          ])
    specHelper validCase22 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 2
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 3
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 4
          ])
    specHelper validCase23 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrSub (Asm.Reg Asm.regRax) $ Asm.Imm 2
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 3
          , Asm.instrSub (Asm.Reg Asm.regRdi) $ Asm.Imm 4
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Reg Asm.regRdi
          ])
    specHelper validCase24 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 2
          , Asm.instrSub (Asm.Reg Asm.regRax) $ Asm.Imm 3
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 1
          , Asm.instrAdd (Asm.Reg Asm.regRax) $ Asm.Imm 4
          ])
    specHelper validCase25 $ Right ([], [], [Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Imm 1])
    specHelper validCase26 $
      Right
        ( []
        , []
        , [ Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 2
          , Asm.instrMov (Asm.Reg Asm.regRsi) $ Asm.Imm 1
          , Asm.instrCall (Asm.Lab "min_caml_create_array")
          , Asm.instrMov (Asm.Reg Asm.regRdi) $ Asm.Imm 2
          , Asm.instrMov (Asm.Mem Asm.regRax 0) $ Asm.Reg Asm.regRdi
          , Asm.instrMov (Asm.Reg Asm.regRax) $ Asm.Mem Asm.regRax 8
          ])
