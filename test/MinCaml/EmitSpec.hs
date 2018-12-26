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
