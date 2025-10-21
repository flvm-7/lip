%{
    open Ast
%}

%token TRUE
%token FALSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token EOF

%left ELSE
%left OR
%left AND
%left NOT
%left ISZERO
%left SUCC PRED

%start <expr> prog

%%

prog:
    | e = expr; EOF { e }
;;

expr:
    | TRUE { True }
    | FALSE { False }
    | ZERO { Zero }
    | SUCC; e = expr { Succ(e) }
    | PRED; e = expr { Pred(e) }
    | ISZERO; e = expr { IsZero(e) }
    | NOT; e = expr { Not(e) }
    | e1 = expr; AND; e2 = expr { And(e1,e2) }
    | e1 = expr; OR; e2 = expr { Or(e1,e2) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1,e2,e3) }
;;