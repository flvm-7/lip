{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let string = ['A'-'Z' 'a'-'z']|['A'-'Z' 'a'-'z']*
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_pref = "0x"|"0X"
let hex_val = hex_pref hex_digit+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "-" { UNARY_MINUS }
  | "+" { PLUS }
  | "- " { LESS }
  | "*" { MULT }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex_val { HEX (Lexing.lexeme lexbuf) }
  | eof { EOF }
