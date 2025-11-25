%{
    open Ast

    let intList_of_string s =
        let ascii_offset = 48 in 
        let rec aux acc = function
            | 0,_ -> []
            | 1,s -> List.rev (((int_of_char s.[0]) - ascii_offset)::acc)
            | l,s -> aux (((int_of_char s.[0]) - ascii_offset)::acc) (l-1, String.sub s 1 (l-1))
        in aux [] (String.length s, s) 

    let intList_of_range l u = 
        let rec aux acc = function
            | (l,u) when l <= u -> aux (l::acc) (l+1,u)
            | _ -> List.rev acc
        in aux [] (l,u)
%}

%token <string> DIGIT
%token S
%token B
%token E
%token SLASH
%token COMMA
%token DOTS
%token EOF

%left S B
%left COMMA

%start <rule> prog

%%

prog:
    | r = rule; EOF { r }
;

rule:
    | S; n1 = DIGIT; SLASH; B; n2 = DIGIT { Rule(intList_of_string n1, intList_of_string n2) }
    | E; e1 = ext_rule; SLASH; e2 = ext_rule { Rule(e1,e2) }  
;

ext_rule:
    | n = DIGIT { intList_of_string n }
    | n1 = DIGIT; DOTS; n2 = DIGIT { intList_of_range (int_of_string n1) (int_of_string n2) }
    | e1 = ext_rule; COMMA; e2 = ext_rule { (@) e1 e2 }
    | S; e = ext_rule { e }
    | B; e = ext_rule { e }
    | S | B { [] } 
;
