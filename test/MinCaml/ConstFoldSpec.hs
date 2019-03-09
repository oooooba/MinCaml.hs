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
    specHelper validCase3 $ Right $ KNormal.Int 42
    specHelper validCase4 $ Right $ KNormal.Int 42
    specHelper validCase5 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 1) $ KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 2) $ KNormal.Int 3
    specHelper validCase6 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 3) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 4) $ KNormal.Int (-1)
    specHelper validCase7 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 5) $ KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 6) $ KNormal.Int 0
    specHelper validCase8 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 7) $ KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 8) $ KNormal.Int 1
    specHelper validCase9 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 9) $ KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 10) $ KNormal.Int 1
    specHelper validCase10 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 12) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 11) $ KNormal.Int 0
    specHelper validCase11 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 14) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 13) $ KNormal.Int 1
    specHelper validCase12 $
      Right $
      KNormal.Let ("Ti0.0", Type.Int) (KNormal.Int 15) $
      KNormal.Let ("Ti1.1", Type.Int) (KNormal.Int 16) $ KNormal.Int 0
    specHelper validCase13 $ Right $ KNormal.Let ("x_.0", Type.Int) (KNormal.Int 42) $ KNormal.Int 42
