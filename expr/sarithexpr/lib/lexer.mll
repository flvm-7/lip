{
    open Parser
}

let white = [' ' '\t']+

rule read =
    parse
    | white { read lexbuf }
    | "true" { TRUE }
    | "false" { FALSE }
    | "0" { ZERO } 
    | "succ" { SUCC }
    | "pred" { PRED }
    | "iszero" { ISZERO }
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | eof { EOF }