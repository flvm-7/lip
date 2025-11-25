{
    open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9'] | ['0'-'9']*
let slash = white* "/" white*
let ext = white* "E" white*

rule read =
    parse
    | white { read lexbuf }
    | digit { DIGIT(Lexing.lexeme lexbuf) }
    | "S" { S }
    | "B" { B }
    | slash { SLASH }
    | ext { E }
    | "," { COMMA }
    | ".."  { DOTS }
    | eof { EOF }