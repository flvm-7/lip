open Ast

type exprval = Bool of bool | Nat of int

let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | Zero -> "Zero"
  | Not(e) -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | If(e1,e2,e3) -> "If(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ "," ^ (string_of_expr e3) ^ ")"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"

let string_of_val = function
  | Bool(p) -> "Bool(" ^ (string_of_bool p) ^ ")"
  | Nat(n) -> "Nat(" ^ (string_of_int n) ^ ")"

let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast 

let is_nv expr = 
  let rec aux acc = function
    | Zero -> if acc >= 0 then true else false
    | Succ(expr) -> aux (acc+1) expr
    | Pred(expr) -> aux (acc-1) expr
    | _ -> false 
  in aux 0 expr

exception NoRuleApplies
exception MalformedSentence
exception NegativeValue

let rec trace1 = function
  | Not(e) -> begin
    e 
    |> function 
      | True -> False
      | False -> True
      | _ -> Not(trace1 e)
    end
  | And(e1,e2) -> begin 
    e1 
    |> function
      | True -> e2
      | False -> False
      | _ -> And(trace1 e1,e2)
    end
  | Or(e1,e2) -> begin
    e1 
    |> function
      | True -> True
      | False -> e2
      | _ -> Or(trace1 e1,e2)
    end
  | If(e1,e2,e3) -> begin 
    e1 
    |> function
      | True -> e2
      | False -> e3
      | _ -> If(trace1 e1,e2,e3) 
    end
  | IsZero(e) -> begin
    e
    |> function 
      | e when (trace1 e) = Zero -> True
      | _ -> False
    end
  | Succ(n) -> begin
    n 
    |> function
      | Pred(n') -> n'
      | _ -> Succ(trace1 n) 
    end
  | Pred(n) ->  begin 
    n
    |> function
      | Succ(n') -> n'
      | _ -> Pred(trace1 n)
    end
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e] 

let rec eval = function 
  | True -> Bool(true)
  | False -> Bool(false)
  | Zero -> Nat(0)
  | Not(e) -> begin
    (eval e) 
    |> function 
      | Bool(p) -> Bool(not p) 
      | _ -> raise MalformedSentence
    end 
  | And(e1,e2) -> begin 
    (eval e1, eval e2) 
    |> function 
      | (Bool(p1),Bool(p2)) -> Bool(p1 && p2) 
      | _ -> raise MalformedSentence 
    end
  | Or(e1,e2) -> begin 
    (eval e1, eval e2) 
    |> function 
      | (Bool(p1),Bool(p2)) -> Bool(p1 || p2) 
      | _ -> raise MalformedSentence
    end
  | If(e1,e2,e3) -> begin
    (eval e1, eval e2, eval e3) 
    |> function 
      | (Bool(p), e1, e2) -> if p then e1 else e2 
      | _ -> raise MalformedSentence
    end
  | IsZero(e) -> begin
    (eval e)
    |> function
      | Nat(n) -> if n = 0 then Bool(true) else Bool(false)
      | _ -> raise MalformedSentence
    end
  | Succ(e) -> begin
    (eval e)
    |> function
      | Nat(n) -> Nat(n+1)
      | _ -> raise MalformedSentence
    end
  | Pred(e) -> begin
    (eval e)
    |> function
      | Nat(n) -> if n > 0 then Nat(n-1) else raise NegativeValue
      | _ -> raise MalformedSentence 
    end
