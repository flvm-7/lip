(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)

module AuxFun = struct
  let is_valid = function
    | 'A'|'B'|'=' -> true
    | _ -> false 

  let token_of_string = function
    | 'A' -> Some (A)
    | 'B' -> Some (B)
    | '=' -> Some (X)
    | _ -> None

  let explode s = 
    let rec exp i l = 
      if i < 0 then l 
      else exp (i-1) (s.[i]::l) 
    in exp (String.length s - 1) []

  let error_message = "Invalid Input"
end

let toklist_of_string s = let open AuxFun in
  let rec aux acc = function
    | [] -> List.rev acc
    | h::t -> begin
      if is_valid h then token_of_string h |> function
        | None -> failwith error_message
        | Some (v) -> aux (v::acc) t
      else failwith error_message
    end
  in aux [] (explode s)

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec aux acc = function
    | [] -> true
    | A::l' -> if acc = A then aux A l' else false
    | B::l' -> if acc = X || acc = B then aux B l' else false
    | _::l' -> if acc = A || acc = X then aux X l' else false
  in aux A l

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win = function 
  | [] -> failwith "Empty List"
  | l -> begin l 
    |> List.map (function | A -> 1 | B -> -1 | X -> 0)
    |> List.fold_left (+) 0
    |> fun x -> 
      if x > 0 then A 
      else if x < 0 then B
      else X 
  end

(* val string_of_winner : token -> string *)
let string_of_winner w = String.make 1 w