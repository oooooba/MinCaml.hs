module MinCaml.KNormal
  ( T(..)
  , f
  ) where

import           Control.Applicative ((<$>))
import qualified Data.Map            as Map

import           MinCaml.Global
import qualified MinCaml.Id          as Id
import qualified MinCaml.Syntax      as Syntax
import qualified MinCaml.Type        as Type

data T
  = Unit
  | Int Int
  deriving (Show, Eq)

g :: Map.Map Id.T Type.Type -> Syntax.T -> MinCaml (T, Type.Type)
g _ Syntax.Unit    = return (Unit, Type.Unit)
g _ (Syntax.Int i) = return (Int i, Type.Int)

f :: Syntax.T -> MinCaml T
f e = fst <$> g Map.empty e
