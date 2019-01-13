import           Test.Hspec

import           System.Exit          (ExitCode (..))
import           System.IO            (hClose, hPutStr)
import           System.Process       (CreateProcess (std_in, std_out),
                                       StdStream (CreatePipe), proc,
                                       waitForProcess, withCreateProcess)

import qualified MinCaml.AlphaSpec    as AlphaSpec
import qualified MinCaml.ClosureSpec  as ClosureSpec
import qualified MinCaml.EmitSpec     as EmitSpec
import qualified MinCaml.KNormalSpec  as KNormalSpec
import qualified MinCaml.LexerSpec    as LexerSpec
import qualified MinCaml.ParserSpec   as ParserSpec
import qualified MinCaml.RegAllocSpec as RegAllocSpec
import qualified MinCaml.SimmSpec     as SimmSpec
import qualified MinCaml.TypingSpec   as TypingSpec
import qualified MinCaml.VirtualSpec  as VirtualSpec

import           MinCaml.TestCase

testCompiler :: TestCase -> IO ()
testCompiler testCase =
  withCreateProcess
    (proc "bash" ["test.sh", name testCase, output testCase]) {std_in = CreatePipe, std_out = CreatePipe} $ \(Just targetStdin) (Just targetStdout) _ process -> do
    hPutStr targetStdin $ input testCase
    hClose targetStdin
    exitCode <- waitForProcess process
    let msg =
          case exitCode of
            ExitSuccess   -> "passed"
            ExitFailure i -> "failed with " ++ show i
    putStrLn $ name testCase ++ ": " ++ msg

integrationTest :: IO ()
integrationTest = do
  testCompiler validCase3
  testCompiler validCase4
  testCompiler validCase5
  testCompiler validCase6
  testCompiler validCase7
  testCompiler validCase8
  testCompiler validCase9
  testCompiler validCase10
  testCompiler validCase11
  testCompiler validCase12
  testCompiler validCase13
  testCompiler validCase14
  testCompiler validCase15

main :: IO ()
main = do
  integrationTest
  hspec LexerSpec.spec
  hspec ParserSpec.spec
  hspec TypingSpec.spec
  hspec KNormalSpec.spec
  hspec AlphaSpec.spec
  hspec ClosureSpec.spec
  hspec VirtualSpec.spec
  hspec SimmSpec.spec
  hspec RegAllocSpec.spec
  hspec EmitSpec.spec
