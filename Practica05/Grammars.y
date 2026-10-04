{
module Grammars where

import Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      var             { TokenId $$ }
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      "not"           { TokenNot }
      "let"           { TokenLet }
      "let*"          { TokenLetStar }
      "lambda"        { TokenLambda }
      "if"            { TokenIf }
      "cond"          { TokenCond }
      "else"          { TokenElse }
      "letrec"        { TokenLetRec }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

SASA : var                               { IdS $1 }
     | nat                               { NumS $1 }
     | bool                              { BooleanS $1 }
     | '(' '+' Operands ')'              { AddS $3 }
     | '(' '-' Operands ')'              { SubS $3 }
     | '(' "not" SASA ')'                { NotS $3 }
     | '(' "let" '(' var SASA ')' SASA ')'
                                         { LetS $4 $5 $7 }
     | '(' "let*" '(' Bindings ')' SASA ')'
                                         { LetStarS $4 $6 }
     | '(' "lambda" '(' Params ')' SASA ')'
                                         { FunS $4 $6 }
     | '(' SASA Arguments ')'            { AppS $2 $3 }

     | '(' "if" SASA SASA SASA ')'       { IfS $3 $4 $5 }
     | '(' "cond" '(' SASA SASA ')' Clauses ')'
                                         { let (clausulas, ramaElse) = $7
                                           in CondS (($4, $5) : clausulas) ramaElse }
     | '(' "letrec" '(' var SASA ')' SASA ')'
                                         { LetRecS $4 $5 $7 }

-- Definición de nuestro terminal Clauses, que representa las cláusulas faltantes de la expresión condicional 
Clauses : '(' "else" SASA ')'            { ([], $3) }
        | '(' SASA SASA ')' Clauses      { let (clausulas, ramaElse) = $5
                                          in (($2, $3) : clausulas, ramaElse) }

Params : var                             { [$1] }
       | var Params                      { $1 : $2 }

Arguments : SASA                         { [$1] }
          | SASA Arguments               { $1 : $2 }

Operands : SASA SASA                     { [$1, $2] }
         | SASA Operands                 { $1 : $2 }

Bindings : '(' var SASA ')'              { [($2, $3)] }
         | '(' var SASA ')' Bindings     { ($2, $3) : $5 }

{
parseError :: [Token] -> a
parseError tokens = error ("Parse error: " ++ show tokens)

type Nombre = String

data SASA
  = IdS Nombre
  | NumS Int
  | BooleanS Bool
  | AddS [SASA]
  | SubS [SASA]
  | NotS SASA
  | LetS Nombre SASA SASA
  | LetStarS [(Nombre, SASA)] SASA
  | FunS [Nombre] SASA
  | AppS SASA [SASA]
  | IfS SASA SASA SASA
  | CondS [(SASA, SASA)] SASA
  | LetRecS Nombre SASA SASA
  deriving (Eq, Show)
}
