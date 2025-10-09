let lang1 w = 
  match w with 
    [] -> false
    | _ -> List.fold_left (&&) true (List.map (fun x -> (x = '0') || (x = '1')) w)

let lang2 w = 
  match w with 
    [] -> true
    | '0'::w' -> List.for_all (fun x -> x = '1') w'
    | _ -> List.for_all (fun x -> x = '1') w  

let lang3 w =
  let rec aux w acc = 
    match w with
      [] -> acc > 2
      | n::w' -> 
          if n = '1' then aux w' acc 
          else if n = '0' then aux w' (acc+1)
          else false 
  in match w with 
    '0'::w' -> aux w' 0
    | _ -> false

let lang4 w = 
  let rec aux w acc = 
    match w with 
      [] -> acc = 2 
      | n::w' -> 
        if n = '0' then aux w' acc
        else if n = '1' then aux w' (acc+1)
        else false
  in aux w 0

let lang5 w = 
  let rec aux w =
    match w with 
      [] -> false
      | '0'::'0'::[] | '1'::'1'::[] -> true
      | n::n'::w' -> 
        if (n = n' && (n = '0' || n = '1')) then aux w'
        else false
      | _ -> false
  in aux w
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
