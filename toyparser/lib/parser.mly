%{
open Ast
%}

%token <string> CONST
%token <string> HEX
%token UNARY_MINUS
%token PLUS
%token LESS
%token MULT
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS LESS
%left MULT DIV
%left UNARY_MINUS

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | h = HEX { Hex(h) }
  | UNARY_MINUS; e = expr { UnMinus(e) } 
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; LESS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MULT; e2 = expr { Mul(e1,e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;
