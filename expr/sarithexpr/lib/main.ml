open Ast

type exprtype = BoolT | NatT

let parse s = 
  let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec string_of_expr = function
  | True -> "true"
  | False -> "false"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "iszero(" ^ (string_of_expr e) ^ ")"
  | Not(e) -> "not(" ^ (string_of_expr e) ^ ")"
  | And(e1,e2) -> "and(" ^ (string_of_expr e1) ^ "," ^  (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | If(e1,e2,e3) -> "if(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ "," ^ (string_of_expr e3) ^ ")"

let string_of_type = function
  | BoolT -> "Bool"
  | NatT -> "Nat"

exception TypeError of string

let string_of_error e t1 t2 = 
  (string_of_expr e) ^ " has type " ^ (string_of_type t1) ^ ", but type " ^ (string_of_type t2) ^ " was expected"

let rec eval = function
  | Zero -> Zero
  | True -> True
  | False -> False
  | Succ(e) -> begin 
    e
    |> function
      | Pred(e') -> eval e'
      | e' -> Succ(eval e')
    end
  | Pred(e) -> begin
    e 
    |> function
      | Succ(e') -> eval e'
      | e' -> Pred(eval e')
    end 
  | IsZero(e) -> if Zero = eval e then True else False
  | Not(e) -> if True = eval e then False else True
  | And(e1,e2) -> if True = eval e1 then eval e2 else False
  | Or(e1,e2) -> if True = eval e1 then True else eval e2
  | If(e1,e2,e3) -> if True = eval e1 then eval e2 else eval e3

let rec typecheck = function
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) | Pred(e) -> begin 
      if NatT = typecheck e then NatT
      else raise (TypeError (string_of_error e BoolT NatT))
    end
  | IsZero(e) -> begin 
      if NatT = typecheck e then BoolT
      else raise (TypeError (string_of_error e BoolT NatT))
    end
  | Not(e) -> begin
      if BoolT = typecheck e then BoolT
      else raise (TypeError (string_of_error e NatT BoolT))
    end 
  | And(e1,e2) | Or(e1,e2) -> begin
    (typecheck e1, typecheck e2)
    |> function
      | (NatT, _) -> raise (TypeError (string_of_error e1 NatT BoolT))
      | (BoolT, NatT) -> raise (TypeError (string_of_error e2 NatT BoolT))
      | _ -> BoolT
    end 
  | If(e1,e2,e3) -> begin 
      let t1 = typecheck e2 in
        let t2 = typecheck e3 in
          (typecheck e1, t1 = t2)
          |> function 
            | (BoolT, true) -> t1
            | (BoolT, false) -> raise (TypeError (string_of_error e2 t1 t2))
            | (NatT, _) -> raise (TypeError (string_of_error e1 NatT BoolT))
    end

exception NoRuleApplies

let rec trace1 = function
  | Succ(e) ->  begin
      e 
      |> function
        | Zero -> Succ(Zero)
        | Pred(e') -> e'
        | _ -> Succ(trace1 e)
    end
  | Pred(e) -> begin 
      e
      |> function
        | Succ(e') -> e'
        | _ -> Pred(trace1 e)
    end
  | IsZero(e) -> begin 
      e 
      |> function
        | Zero -> True
        | Succ(e') -> if Zero = eval (Succ(e')) then True else False
        | Pred(e') -> if Zero = eval (Pred(e')) then True else False 
        | _ -> IsZero(trace1 e)
    end   
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
  | _ -> raise NoRuleApplies

let rec trace e = try
  let e' = trace1 e 
    in e::(trace e')
  with NoRuleApplies -> [e]
