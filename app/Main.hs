module Main where

import           System.Environment (getArgs)
import           System.IO          (Handle, IOMode (ReadMode, WriteMode),
                                     hGetContents, hPutStr, openFile, stderr,
                                     stdin, stdout, withFile)

import qualified MinCaml.Alpha      as Alpha
import qualified MinCaml.Asm        as Asm
import qualified MinCaml.Closure    as Closure
import qualified MinCaml.Emit       as Emit
import           MinCaml.Global
import qualified MinCaml.Id         as Id
import qualified MinCaml.KNormal    as KNormal
import qualified MinCaml.Lexer      as Lexer
import qualified MinCaml.Parser     as Parser
import qualified MinCaml.RegAlloc   as RegAlloc
import qualified MinCaml.Simm       as Simm
import qualified MinCaml.Type       as Type
import qualified MinCaml.Typing     as Typing
import qualified MinCaml.Virtual    as Virtual

compile :: String -> Either String [String]
compile program =
  evalMinCaml
    (Parser.runParser (Lexer.runLexer program) >>= Typing.f >>= KNormal.f . fst >>= Alpha.f >>= Closure.f >>= Virtual.f >>=
     Simm.f >>=
     RegAlloc.f >>=
     Emit.f')
    initialGlobalStatus

process :: Handle -> Handle -> IO ()
process inputHandle outputHandle = do
  program <- hGetContents inputHandle
  case compile program of
    Left error -> hPutStr stderr error
    Right asm  -> hPutStr outputHandle $ unlines asm

file :: String -> IO ()
file inputPath = do
  let outputPath = inputPath ++ ".s"
  withFile inputPath ReadMode (withFile outputPath WriteMode . process)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then process stdin stdout
    else mapM_ file args
