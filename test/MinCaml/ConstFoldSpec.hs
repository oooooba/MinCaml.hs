module MinCaml.ConstFoldSpec
  ( spec
  ) where

import           Test.Hspec

import qualified MinCaml.Alpha     as Alpha
import qualified MinCaml.Assoc     as Assoc
import qualified MinCaml.Beta      as Beta
import qualified MinCaml.ConstFold as ConstFold
import           MinCaml.Global
import qualified MinCaml.Inline    as Inline
import qualified MinCaml.KNormal   as KNormal
import qualified MinCaml.Lexer     as Lexer
import qualified MinCaml.Parser    as Parser
import qualified MinCaml.Type      as Type
import qualified MinCaml.Typing    as Typing

import           MinCaml.TestCase

specHelper :: TestCase -> Either String KNormal.T -> Spec
specHelper testCase expected =
  it (name testCase) $
  evalMinCaml
    ((Parser.runParser . Lexer.runLexer $ input testCase) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Beta.f >>=
     Assoc.f >>=
     Inline.f >>=
     ConstFold.f)
    initialGlobalStatus `shouldBe`
  expected

spec :: Spec
spec =
  describe "valid cases" $ do
    specHelper validCase1 $ Right KNormal.Unit
    specHelper validCase2 $ Right KNormal.Unit
