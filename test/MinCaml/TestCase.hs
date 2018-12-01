module MinCaml.TestCase where

data TestCase = TestCase
  { name  :: String
  , input :: String
  } deriving (Show)

validCase1 = TestCase {name = "validCase1", input = "()"}

validCase2 = TestCase {name = "validCase2", input = "(())"}

validCase3 = TestCase {name = "validCase3", input = "42"}

validCase4 = TestCase {name = "validCase4", input = "(42)"}

validCase5 = TestCase {name = "validCase5", input = "1+2"}

validCase6 = TestCase {name = "validCase6", input = "3-4"}

validCase7 = TestCase {name = "validCase7", input = "5=6"}

validCase8 = TestCase {name = "validCase8", input = "7<>8"}

validCase9 = TestCase {name = "validCase9", input = "9<=10"}

validCase10 = TestCase {name = "validCase10", input = "11>=12"}

validCase11 = TestCase {name = "validCase11", input = "13<14"}

validCase12 = TestCase {name = "validCase12", input = "15>16"}

validCase13 = TestCase {name = "validCase13", input = "let x_ = 42 in x_"}

validCase14 = TestCase {name = "validCase14", input = "-1=-2-3"}

validCase15 = TestCase {name = "validCase15", input = "if true then false else not true"}

validCase16 = TestCase {name = "validCase16", input = "let rec f x = x + 1 in 2"}

validCase17 = TestCase {name = "validCase17", input = "let rec f x = x + 1 in f 2"}

validCase18 = TestCase {name = "validCase18", input = "let rec f x y = x + y in f 1 2"}

validCase19 = TestCase {name = "validCase19", input = "let rec f n = if n <= 0 then 0 else n + f (n - 1) in f 5"}
