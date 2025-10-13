module AuxFun = struct
  let is_one_or_zero x = x = '0' || x = '1'

  let is_one = (=) '1'

  let rec aux_lang3 = function
    | [] -> false
    | '0'::[] -> true
    | h::t -> if is_one_or_zero h then aux_lang3 t else false

  let transform_ones x = if x = '1' then 1 else 0 

  let count_ones w = List.fold_left ( + ) 0 (List.map transform_ones w)

  let rec aux_lang5 = function
    | [] -> true
    | n::n'::w -> if (n = n') && is_one_or_zero n then aux_lang5 w else false
    | _ -> false
end

let lang1 = let open AuxFun in function
  | [] -> false
  | w -> List.fold_left (&&) true (List.map is_one_or_zero w)

let lang2 = let open AuxFun in function 
  | [] -> true
  | '0'::w' -> List.for_all is_one w'
  | w -> List.for_all is_one w  

let lang3 = let open AuxFun in function
  | '0'::t -> aux_lang3 t
  | _ -> false

let lang4 = let open AuxFun in function
  | [] -> false
  | w -> begin
      if (List.fold_left ( && ) true (List.map is_one_or_zero w)) then 
        (count_ones w) = 2
      else false
    end

let lang5 = let open AuxFun in function
  | [] -> false
  | _::[] -> false
  | w -> aux_lang5 w 
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
