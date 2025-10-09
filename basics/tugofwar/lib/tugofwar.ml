(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s = 
  let explode s = 
    let rec exp i l = 
      if i < 0 then l else exp (i-1) (s.[i]::l) in
    exp (String.length s - 1) [] in
  let rec aux l acc = 
    match l with 
      [] -> acc
      | 'A'::l' -> aux l' (acc@'A'::[])
      | '='::l' -> aux l' (acc@'X'::[])
      | 'B'::l' -> aux l' (acc@'B'::[])
      | _ -> failwith "Invalid Input" in
  aux (explode s) []

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec aux l acc = 
    match l with 
      [] -> true
      | 'A'::l' -> if acc = 'A' then aux l' 'A' else false
      | 'X'::l' -> if acc = 'A' || acc = 'X' then aux l' 'X' else false
      | 'B'::l' -> if acc = 'X' || acc = 'B' then aux l' 'B' else false
      | _ -> false in
  aux l 'A'

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let nA = List.length (List.filter (fun x -> x = 'A') l) in
  let nB = List.length (List.filter (fun x -> x = 'B') l) in
  if nA = nB then 'X' else if nA > nB then 'A' else 'B'

(* val string_of_winner : token -> string *)
let string_of_winner w = String.make 1 w