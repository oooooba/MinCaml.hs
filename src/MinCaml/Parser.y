{
module MinCaml.Parser (runParser) where

import MinCaml.Global
import MinCaml.Lexer
import MinCaml.Syntax
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  '('  { LPAREN }
  ')'  { RPAREN }
  bool { BOOL $$ }
  int  { INT $$ }
  '+'  { PLUS }
  '-'  { MINUS }
  '='  { EQUAL }
  '<>' { LESS_GREATER }
  '<=' { LESS_EQUAL }
  '>=' { GREATER_EQUAL }
  '<'  { LESS }
  '>'  { GREATER }
  if   { IF }
  then { THEN }
  else { ELSE }

%right prec_if
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%right prec_unary_minus

%%

exp : simple_exp  { $1 }
    | '-' exp %prec prec_unary_minus         { Neg $2 }
    | exp '+' exp                            { Add $1 $3 }
    | exp '-' exp                            { Sub $1 $3 }
    | exp '=' exp                            { Eq $1 $3 }
    | exp '<>' exp                           { Not $ Eq $1 $3 }
    | exp '<' exp                            { Not $ Le $3 $1 }
    | exp '>' exp                            { Not $ Le $1 $3 }
    | exp '<=' exp                           { Le $1 $3 }
    | exp '>=' exp                           { Le $3 $1 }
    | if exp then exp else exp %prec prec_if { If $2 $4 $6 }

simple_exp : '(' exp ')' { $2 }
           | '(' ')'     { Unit }
           | bool        { Bool $1 }
           | int         { Int $1 }

{
runParser :: [Token] -> MinCaml T
runParser tokens = do
  let exp = parser tokens
  return exp

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}