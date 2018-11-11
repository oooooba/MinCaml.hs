module MinCaml.TestCase where

data TestCase = TestCase
  { name  :: String
  , input :: String
  } deriving (Show)

validCase1 = TestCase {name = "validCase1", input = "()"}

validCase2 = TestCase {name = "validCase2", input = "(())"}

validCase3 = TestCase {name = "validCase3", input = "42"}

validCase4 = TestCase {name = "validCase4", input = "(42)"}
