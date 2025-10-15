open Ast

let rec string_of_boolexpr = function
  | True -> "True"
  | False -> "False"
  | Not(e) -> "Not(" ^ (string_of_boolexpr e) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | If(e1,e2,e3) -> "If(" ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ "," ^ (string_of_boolexpr e3) ^ ")"

let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function 
  | Not(e) -> begin 
    if e = True then False 
    else if e = False then True 
    else (Not(trace1 e))
    end
  | And(e1,e2) -> begin 
    match (e1,e2) with
      | (True, _) -> e2
      | (False,_) -> False
      | (e1',_) -> And(trace1 e1', e2)
    end
  | Or(e1,e2) -> begin
      match (e1,e2) with
      | (True, _) -> True
      | _ -> e2
    end
  | If(e1,e2,e3) -> begin
    match (e1,e2,e3) with
      | (True,e',_) | (False,_,e') -> e'
      | _ -> If(trace1 e1,e2,e3)
    end
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e 
    in e::(trace e')
  with NoRuleApplies -> [e]

  

let rec eval = function
  | True -> true
  | False -> false
  | Not(e) -> not (eval e)
  | And(e1,e2) -> (eval e1) && (eval e2)
  | Or(e1,e2) -> (eval e1) || (eval e2)
  | If(e1,e2,e3) -> if eval e1 then eval e2 else eval e3