module MinCaml.Type where

type TypeVarId = Int

data Type
  = Unit
  | Bool
  | Int
  | Fun [Type]
        Type
  | Tuple [Type]
  | Array Type
  | Var TypeVarId
  deriving (Show, Eq)
