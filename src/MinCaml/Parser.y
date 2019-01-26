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
  '('          { LPAREN }
  ')'          { RPAREN }
  bool         { BOOL $$ }
  int          { INT $$ }
  not          { NOT }
  '+'          { PLUS }
  '-'          { MINUS }
  '='          { EQUAL }
  '<>'         { LESS_GREATER }
  '<='         { LESS_EQUAL }
  '>='         { GREATER_EQUAL }
  '<'          { LESS }
  '>'          { GREATER }
  if           { IF }
  then         { THEN }
  else         { ELSE }
  let          { LET }
  in           { IN }
  rec          { REC }
  ident        { IDENT $$ }
  ';'          { SEMICOLON }
  'Array.make' { ARRAY_MAKE }
  '.'          { DOT }
  '<-'         { LESS_MINUS }

%nonassoc in
%right prec_let
%right ';'
%right prec_if
%right '<-'
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%right prec_unary_minus
%left prec_app
%left '.'

%%

exp : simple_exp                                        { $1 }
    | not exp %prec prec_app                            { Not $2 }
    | '-' exp %prec prec_unary_minus                    { Neg $2 }
    | exp '+' exp                                       { Add $1 $3 }
    | exp '-' exp                                       { Sub $1 $3 }
    | exp '=' exp                                       { Eq $1 $3 }
    | exp '<>' exp                                      { Not $ Eq $1 $3 }
    | exp '<' exp                                       { Not $ Le $3 $1 }
    | exp '>' exp                                       { Not $ Le $1 $3 }
    | exp '<=' exp                                      { Le $1 $3 }
    | exp '>=' exp                                      { Le $3 $1 }
    | if exp then exp else exp %prec prec_if            { If $2 $4 $6 }
    | let ident '=' exp in exp %prec prec_let           { Let (addTmpType $2) $4 $6 }
    | let rec fundef in exp %prec prec_let              { LetRec $3 $5 }
    | simple_exp actual_args %prec prec_app             { App $1 $2 }
    | simple_exp '.' '(' exp ')' '<-' exp               { Put $1 $4 $7 }
    | exp ';' exp                                       { Let ("_", Type.Unit) $1 $3 }
    | 'Array.make' simple_exp simple_exp %prec prec_app { Array $2 $3 }

simple_exp : '(' exp ')'                { $2 }
           | '(' ')'                    { Unit }
           | bool                       { Bool $1 }
           | int                        { Int $1 }
           | ident                      { Var $1 }
           | simple_exp '.' '(' exp ')' { Get $1 $4 }

fundef : ident formal_args '=' exp { Fundef (addTmpType $1) $2 $4 }

formal_args : ident formal_args { addTmpType $1 : $2 }
            | ident             { [addTmpType $1] }

actual_args : actual_args simple_exp %prec prec_app { $1 ++ [$2] }
            | simple_exp %prec prec_app             { [$1] }

{
runParser :: [Token] -> MinCaml T
runParser tokens = do
  let exp = parser tokens
  exp' <- addType exp
  replaceVar exp'

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
  yts <- mapM (\yt -> if snd yt /= Type.Var (-1) then error "addType" else genType >>= (\t -> return (fst yt, t))) $ args fundef
  body' <- addType $ body fundef
  e' <- addType e
  return $ LetRec (Fundef (x, xt) yts body') e'
addType (App e es) = liftM2 App (addType e) (mapM addType es)
addType e = return e

replaceVarHelper :: (Id.T, Type.Type) -> MinCaml (Id.T, Type.Type)
replaceVarHelper ("_", t) = liftM2 (,) (genVar t) $ return t
replaceVarHelper xt = return xt

replaceVar :: T -> MinCaml T
replaceVar (Not e) = liftM Not $ replaceVar e
replaceVar (Neg e) = liftM Neg $ replaceVar e
replaceVar (Add e1 e2) = liftM2 Add (replaceVar e1) (replaceVar e2)
replaceVar (Sub e1 e2) = liftM2 Sub (replaceVar e1) (replaceVar e2)
replaceVar (Eq e1 e2) = liftM2 Eq (replaceVar e1) (replaceVar e2)
replaceVar (Le e1 e2) = liftM2 Le (replaceVar e1) (replaceVar e2)
replaceVar (If e1 e2 e3) = liftM3 If (replaceVar e1) (replaceVar e2) (replaceVar e3)
replaceVar (Let xt e1 e2) = liftM3 Let (replaceVarHelper xt) (replaceVar e1) (replaceVar e2)
replaceVar (Let (x, t) e1 e2) = liftM2 (Let (x, t)) (replaceVar e1) (replaceVar e2)
replaceVar (Var "_") = error "Parse error: invalid identifier ('_')"
replaceVar (LetRec fundef e) = do
  xt <- replaceVarHelper $ name fundef
  yts <- mapM replaceVarHelper $ args fundef
  body' <- replaceVar $ body fundef
  e' <- replaceVar e
  return $ LetRec (Fundef xt yts body') e'
replaceVar (App e es) = liftM2 App (replaceVar e) (mapM replaceVar es)
replaceVar e = return e

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens
}