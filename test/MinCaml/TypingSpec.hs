module MinCaml.TypingSpec
  ( spec
  ) where

import           Test.Hspec

import           MinCaml.Global
import qualified MinCaml.Syntax as Syntax
import qualified MinCaml.Type   as Type
import qualified MinCaml.Typing as Typing

typing :: Syntax.T -> MinCaml (Syntax.T, Type.Type)
typing e = do
  t <- genType
  let topLevel = Syntax.Let ("_", t) e Syntax.Unit
  Typing.f topLevel

expected :: Type.Type -> Syntax.T -> Syntax.T
expected t e = Syntax.Let ("_", t) e Syntax.Unit

spec :: Spec
spec =
  describe "typing" $ do
    it "unit literal" $
      evalMinCaml (typing Syntax.Unit) initialGlobalStatus `shouldBe` Right (expected Type.Unit Syntax.Unit, Type.Unit)
    it "integer literal with let" $
      evalMinCaml (typing $ Syntax.Int 42) initialGlobalStatus `shouldBe`
      Right (expected Type.Int (Syntax.Int 42), Type.Unit)
    it "integer literal" $
      evalMinCaml (Typing.f $ Syntax.Int 42) initialGlobalStatus `shouldBe` Right (Syntax.Int 42, Type.Int)
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
      Right
        ( expected
            Type.Int
            (Syntax.Let
               ("x", Type.Int)
               (Syntax.Int 1)
               (Syntax.Let ("y", Type.Int) (Syntax.Int 2) (Syntax.Add (Syntax.Var "x") (Syntax.Var "y"))))
        , Type.Unit)
