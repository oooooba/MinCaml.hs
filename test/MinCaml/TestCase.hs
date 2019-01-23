module MinCaml.TestCase where

data TestCase = TestCase
  { name   :: String
  , input  :: String
  , output :: String
  } deriving (Show)

validCase1 = TestCase {name = "validCase1", input = "()", output = ""}

validCase2 = TestCase {name = "validCase2", input = "(())", output = ""}

validCase3 = TestCase {name = "validCase3", input = "42", output = "42"}

validCase4 = TestCase {name = "validCase4", input = "(42)", output = "42"}

validCase5 = TestCase {name = "validCase5", input = "1+2", output = "3"}

validCase6 = TestCase {name = "validCase6", input = "3-4", output = "-1"}

validCase7 = TestCase {name = "validCase7", input = "5=6", output = "0"}

validCase8 = TestCase {name = "validCase8", input = "7<>8", output = "1"}

validCase9 = TestCase {name = "validCase9", input = "9<=10", output = "1"}

validCase10 = TestCase {name = "validCase10", input = "11>=12", output = "0"}

validCase11 = TestCase {name = "validCase11", input = "13<14", output = "1"}

validCase12 = TestCase {name = "validCase12", input = "15>16", output = "0"}

validCase13 = TestCase {name = "validCase13", input = "let x_ = 42 in x_", output = "42"}

validCase14 = TestCase {name = "validCase14", input = "-1=-2-3", output = "0"}

validCase15 = TestCase {name = "validCase15", input = "if true then false else not true", output = "0"}

validCase16 = TestCase {name = "validCase16", input = "let rec f x = x + 1 in 2", output = "2"}

validCase17 = TestCase {name = "validCase17", input = "let rec f x = x + 1 in f 2", output = "3"}

validCase18 = TestCase {name = "validCase18", input = "let rec f x y = x + y in f 1 2", output = "3"}

validCase19 =
  TestCase {name = "validCase19", input = "let rec f n = if n <= 0 then 0 else n + f (n - 1) in f 5", output = "15"}

validCase20 = TestCase {name = "validCase20", input = "let rec f x = let rec g y = x + y in g in (f 1) 2", output = "3"}

validCase21 =
  TestCase
  { name = "validCase21"
  , input = "let x = 1 in let rec f y = x + y in let rec g z = z + 2 in (if 3 = 4 then f else g) 5"
  , output = "7"
  }

validCase22 = TestCase {name = "validCase22", input = "1+2+3+4", output = "10"}

validCase23 = TestCase {name = "validCase23", input = "(1-2)+(3-4)", output = "-2"}

validCase24 = TestCase {name = "validCase24", input = "1+(2-3)+4", output = "4"}

validCase25 = TestCase {name = "validCase25", input = "let _ = 42 in ((); 1)", output = "1"}
