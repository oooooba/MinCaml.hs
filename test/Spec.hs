import           Test.Hspec

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

main :: IO ()
main = do
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
