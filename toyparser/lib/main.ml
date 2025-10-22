open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type int_or_err = (int, string) Result.t

let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

exception NotHexVal

let is_number c = 
    let ascii_val = int_of_char c 
  in ascii_val |> function
    | v when (v > 47) && (v < 58) -> true
    | _ -> false

let int_of_hex = 
    let ascii_offset = 48 
  in function
  | v when is_number v -> (int_of_char v) - ascii_offset
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | _ -> raise NotHexVal

let int_of_hexstring s = 
  let l = String.length s in
  let rec aux acc len = function
    | i when i = len -> acc
    | i -> aux (acc*16 + 
                try int_of_hex s.[i]
                with NotHexVal -> 0) len (i+1)
  in aux 0 l 0

(* eval : ast -> result *)
let rec eval : ast -> int_or_err = function
  | Const n -> Ok n
  | Hex h -> Ok (int_of_hexstring h)
  | UnMinus e -> eval e ==> fun v -> Ok (-v)
  | Add (e1,e2) ->
      eval e1 ==> fun v1 ->
      eval e2 ==> fun v2 ->
      Ok (v1 + v2)
  | Sub (e1,e2) -> 
      eval e1 ==> fun v1 ->
      eval e2 ==> fun v2 ->
      Ok (v1 - v2)
  | Mul (e1,e2) -> 
      eval e1 ==> fun v1 ->
      eval e2 ==> fun v2 ->
      Ok (v1 * v2)
  | Div (e1,e2) -> 
      eval e1 ==> fun v1 ->
      eval e2 ==> fun v2 ->
        if v2 = 0 then Error ("Error: tried to divide " ^ (string_of_int v1) ^ " by zero")
        else Ok (v1 / v2)