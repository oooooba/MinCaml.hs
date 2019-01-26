module MinCaml.Id where

import qualified MinCaml.Type as Type

type T = String

newtype L =
  L String
  deriving (Show, Eq)

idOfType :: Type.Type -> String
idOfType Type.Unit      = "u"
idOfType Type.Bool      = "b"
idOfType Type.Int       = "i"
idOfType (Type.Fun _ _) = "f"
idOfType (Type.Tuple _) = "t"
idOfType (Type.Array _) = "a"
idOfType (Type.Var _)   = "v"
