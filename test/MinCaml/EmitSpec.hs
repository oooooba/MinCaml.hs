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

specHelper :: TestCase -> Either String ([[String]], [[String]], [[String]]) -> Spec
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
    specHelper validCase3 $ Right ([], [], [["movl", "42", ",", Asm.regEax]])
    specHelper validCase4 $ Right ([], [], [["movl", "42", ",", Asm.regEax]])
    specHelper validCase5 $ Right ([], [], [["movl", "1", ",", Asm.regEax], ["addl", "$2", ",", Asm.regEax]])
    specHelper validCase6 $ Right ([], [], [["movl", "3", ",", Asm.regEax], ["subl", "$4", ",", Asm.regEax]])
    specHelper validCase7 $
      Right
        ( []
        , []
        , [ ["movl", "5", ",", Asm.regEax]
          , ["cmpl", "$6", ",", Asm.regEax]
          , ["jne", "je_else.2"]
          , ["movl", "1", ",", Asm.regEax]
          , ["jmp", "je_cont.3"]
          , ["je_else.2:"]
          , ["movl", "0", ",", Asm.regEax]
          , ["je_cont.3:"]
          ])
    specHelper validCase8 $
      Right
        ( []
        , []
        , [ ["movl", "7", ",", Asm.regEax]
          , ["cmpl", "$8", ",", Asm.regEax]
          , ["jne", "je_else.2"]
          , ["movl", "0", ",", Asm.regEax]
          , ["jmp", "je_cont.3"]
          , ["je_else.2:"]
          , ["movl", "1", ",", Asm.regEax]
          , ["je_cont.3:"]
          ])
    specHelper validCase9 $
      Right
        ( []
        , []
        , [ ["movl", "9", ",", Asm.regEax]
          , ["cmpl", "$10", ",", Asm.regEax]
          , ["jg", "jle_else.2"]
          , ["movl", "1", ",", Asm.regEax]
          , ["jmp", "jle_cont.3"]
          , ["jle_else.2:"]
          , ["movl", "0", ",", Asm.regEax]
          , ["jle_cont.3:"]
          ])
    specHelper validCase10 $
      Right
        ( []
        , []
        , [ ["movl", "12", ",", Asm.regEax]
          , ["cmpl", "$11", ",", Asm.regEax]
          , ["jg", "jle_else.2"]
          , ["movl", "1", ",", Asm.regEax]
          , ["jmp", "jle_cont.3"]
          , ["jle_else.2:"]
          , ["movl", "0", ",", Asm.regEax]
          , ["jle_cont.3:"]
          ])
    specHelper validCase11 $
      Right
        ( []
        , []
        , [ ["movl", "14", ",", Asm.regEax]
          , ["cmpl", "$13", ",", Asm.regEax]
          , ["jg", "jle_else.2"]
          , ["movl", "0", ",", Asm.regEax]
          , ["jmp", "jle_cont.3"]
          , ["jle_else.2:"]
          , ["movl", "1", ",", Asm.regEax]
          , ["jle_cont.3:"]
          ])
    specHelper validCase12 $
      Right
        ( []
        , []
        , [ ["movl", "15", ",", Asm.regEax]
          , ["cmpl", "$16", ",", Asm.regEax]
          , ["jg", "jle_else.2"]
          , ["movl", "0", ",", Asm.regEax]
          , ["jmp", "jle_cont.3"]
          , ["jle_else.2:"]
          , ["movl", "1", ",", Asm.regEax]
          , ["jle_cont.3:"]
          ])
    specHelper validCase13 $ Right ([], [], [["movl", "42", ",", Asm.regEax]])
