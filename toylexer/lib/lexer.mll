{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*


let capital_letter = ['A'-'Z']
let vowel = ['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']
let consonants = ['a'-'d' 'A'-'D' 'f'-'h' 'F'-'H' 'l'-'n' 'L'-'N' 'p'-'t' 'P'-'T' 'v'-'z' 'V'-'Z']
let hex_pref = "0x"|"0X"
let hex = ['a'-'f' 'A'-'F' '0'-'9']

let a_token = capital_letter chr*
let b_token = vowel+
let c_token = consonants* vowel consonants*
let d_token = (('-')? (num)? '.' (num)?) | ('-' (num)? ('.')? (num)?)
let e_token = hex_pref hex+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | a_token { ATOK (Lexing.lexeme lexbuf) }
  | b_token { BTOK (Lexing.lexeme lexbuf) }
  | c_token { CTOK (Lexing.lexeme lexbuf) }
  | d_token { DTOK (Lexing.lexeme lexbuf) }
  | e_token { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
