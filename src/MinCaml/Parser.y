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
  '+'  { PLUS }
  '-'  { MINUS }

%left '+' '-'
%right prec_unary_minus

%%

exp : simple_exp  { $1 }
    | '-' exp %prec prec_unary_minus { Neg $2 }
    | exp '+' exp                    { Add $1 $3 }
    | exp '-' exp                    { Sub $1 $3 }

simple_exp : '(' exp ')' { $2 }
           | '(' ')'     { Unit }
           | bool        { Bool $1 }
           | int         { Int $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}