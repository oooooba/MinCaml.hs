{
module MinCaml.Parser (runParser) where

import MinCaml.Lexer
import MinCaml.Syntax
}

%name runParser
%tokentype { Token }
%error { parseError }

%token
  '('  { LPAREN }
  ')'  { RPAREN }
  bool { BOOL $$ }
  int  { INT $$ }

%%

simple_exp : '(' exp ')' { $2 }
           | '(' ')'     { Unit }
           | bool        { Bool $1 }
           | int         { Int $1 }

exp : simple_exp { $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}