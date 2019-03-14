module Lib
  ( load
  , optimize
  ) where

import qualified MinCaml.Alpha     as Alpha
import qualified MinCaml.Assoc     as Assoc
import qualified MinCaml.Beta      as Beta
import qualified MinCaml.ConstFold as ConstFold
import qualified MinCaml.Elim      as Elim
import           MinCaml.Global
import qualified MinCaml.Inline    as Inline
import qualified MinCaml.KNormal   as KNormal
import qualified MinCaml.Lexer     as Lexer
import qualified MinCaml.Parser    as Parser
import qualified MinCaml.Type      as Type
import qualified MinCaml.Typing    as Typing

load :: String -> MinCaml KNormal.T
load program = (Parser.runParser . Lexer.runLexer $ program) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f

optimize :: Int -> KNormal.T -> MinCaml KNormal.T
optimize 0 e = return e
optimize limit e = do
  e' <- Beta.f e >>= Assoc.f >>= Inline.f >>= ConstFold.f >>= Elim.f
  if e /= e'
    then optimize (limit - 1) e'
    else return e
