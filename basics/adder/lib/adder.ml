(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

let addlist l = 
  let rec aux l acc = 
    match l with 
    [] -> acc
    | w::l' -> aux l' (acc+w) in
    aux l 0
