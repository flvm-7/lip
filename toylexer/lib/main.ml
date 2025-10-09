open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency (n : int) (tl : 'a list) = 
  let rec aux (l : 'a list) (n : int) (acc : ('a * int) list) = 
    match (l,n>0) with 
      ([],_) -> acc
      | (_, false) -> acc
      | (t::l',true) -> aux (List.filter (fun x -> x <> t) l') (n-1) acc@[(t,List.length (List.filter (fun x -> x=t) (t::l')))] in
  List.rev (List.sort (fun (_,tn) (_,tn') -> compare tn tn') (aux tl n []))