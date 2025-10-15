%{
    open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token IF
%token THEN 
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token IS_ZERO
%token EOF

%left ELSE
%left OR
%left AND
%left NOT
%left IS_ZERO
%left SUCC PRED

%start <expr> prog

%%

prog:
    | e = expr; EOF { e }
;

expr:
    | TRUE { True }
    | FALSE { False }
    | LPAREN; e = expr; RPAREN { e }
    | NOT; e = expr { Not(e) }
    | e1 = expr; AND; e2 = expr { And(e1,e2) }
    | e1 = expr; OR; e2 = expr { Or(e1,e2) }
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1,e2,e3) }
    | ZERO { Zero }
    | SUCC; e = expr { Succ(e) }
    | PRED; e = expr { Pred(e) }
    | IS_ZERO; e = expr { IsZero(e) }