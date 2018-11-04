module MinCaml.TypingSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Syntax as Syntax
import qualified MinCaml.Type   as Type
import qualified MinCaml.Typing as Typing

typing :: Syntax.T -> MinCaml Syntax.T
typing e = do
  t <- genType
  let topLevel = Syntax.Let ("_", t) e Syntax.Unit
  Typing.f topLevel

expected :: Type.Type -> Syntax.T -> Syntax.T
expected t e = Syntax.Let ("_", t) e Syntax.Unit

spec :: Spec
spec = do
  describe "typing" $ do
    it "unit literal" $ evalMinCaml (typing Syntax.Unit) initialGlobalStatus `shouldBe` expected Type.Unit Syntax.Unit
    it "integer literal" $
      evalMinCaml (typing $ Syntax.Int 42) initialGlobalStatus `shouldBe` expected Type.Int (Syntax.Int 42)
    it "integer add" $
      evalMinCaml
        (typing =<< do
           t1 <- genType
           t2 <- genType
           return $
             Syntax.Let
               ("x", t1)
               (Syntax.Int 1)
               (Syntax.Let ("y", t2) (Syntax.Int 2) (Syntax.Add (Syntax.Var "x") (Syntax.Var "y"))))
        initialGlobalStatus `shouldBe`
      expected
        Type.Int
        (Syntax.Let
           ("x", Type.Int)
           (Syntax.Int 1)
           (Syntax.Let ("y", Type.Int) (Syntax.Int 2) (Syntax.Add (Syntax.Var "x") (Syntax.Var "y"))))
