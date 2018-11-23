{
module MinCaml.Parser (runParser) where

import           Control.Monad  (liftM, liftM2, liftM3)

import           MinCaml.Global
import qualified MinCaml.Id     as Id
import           MinCaml.Lexer
import           MinCaml.Syntax
import qualified MinCaml.Type   as Type
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  '('   { LPAREN }
  ')'   { RPAREN }
  bool  { BOOL $$ }
  int   { INT $$ }
  '+'   { PLUS }
  '-'   { MINUS }
  '='   { EQUAL }
  '<>'  { LESS_GREATER }
  '<='  { LESS_EQUAL }
  '>='  { GREATER_EQUAL }
  '<'   { LESS }
  '>'   { GREATER }
  if    { IF }
  then  { THEN }
  else  { ELSE }
  let   { LET }
  in    { IN }
  ident { IDENT $$ }

%nonassoc in
%right prec_let
%right prec_if
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%right prec_unary_minus

%%

exp : simple_exp  { $1 }
    | '-' exp %prec prec_unary_minus          { Neg $2 }
    | exp '+' exp                             { Add $1 $3 }
    | exp '-' exp                             { Sub $1 $3 }
    | exp '=' exp                             { Eq $1 $3 }
    | exp '<>' exp                            { Not $ Eq $1 $3 }
    | exp '<' exp                             { Not $ Le $3 $1 }
    | exp '>' exp                             { Not $ Le $1 $3 }
    | exp '<=' exp                            { Le $1 $3 }
    | exp '>=' exp                            { Le $3 $1 }
    | if exp then exp else exp %prec prec_if  { If $2 $4 $6 }
    | let ident '=' exp in exp %prec prec_let { Let (addTmpType $2) $4 $6 }

simple_exp : '(' exp ')' { $2 }
           | '(' ')'     { Unit }
           | bool        { Bool $1 }
           | int         { Int $1 }
           | ident       { Var $1 }

{
runParser :: [Token] -> MinCaml T
runParser tokens = do
  let exp = parser tokens
  addType exp

addTmpType :: String -> (Id.T, Type.Type)
addTmpType x = (x, Type.Var $ -1)

addType :: T -> MinCaml T
addType (Not e) = liftM Not $ addType e
addType (Neg e) = liftM Neg $ addType e
addType (Add e1 e2) = liftM2 Add (addType e1) (addType e2)
addType (Sub e1 e2) = liftM2 Sub (addType e1) (addType e2)
addType (Eq e1 e2) = liftM2 Eq (addType e1) (addType e2)
addType (Le e1 e2) = liftM2 Le (addType e1) (addType e2)
addType (If e1 e2 e3) = liftM3 If (addType e1) (addType e2) (addType e3)
addType (Let (x, Type.Var (-1)) e1 e2) = do
  t <- genType
  liftM2 (Let (x, t)) (addType e1) (addType e2)
addType (LetRec fundef e) = do
  let (x, Type.Var (-1)) = name fundef
  xt <- genType
  yts <- mapM (\yt -> if snd yt == Type.Var (-1) then error "addType" else genType >>= (\t -> return (fst yt, t))) $ args fundef
  body' <- addType $ body fundef
  e' <- addType e
  return $ LetRec (Fundef (x, xt) yts body') e'
addType (App e es) = liftM2 App (addType e) (mapM addType es)
addType e = return e

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}