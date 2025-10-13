open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

let test_trace1 expr exp_result = 
  (expr |> parse |> trace1 |> trace) = exp_result  

(* ### Unit tests for task 4 *)

let%test "test_eval_1" = 
  test_eval "false" false

let%test "test_eval_2" = 
  test_eval "true" true

let%test "test_eval_3" = 
  test_eval "if true then false else true" false

let%test "test_eval_4" = 
  test_eval "if false then false else true" true

let%test "test_eval_5" = 
  test_eval "if true then (if true then false else true) else (if true then true else false)" false

let%test "test_eval_6" = 
  test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false

let%test "test_eval_7" =
  test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = 
  test_trace1 "if true then (if true then false else true) else (if true then true else false)" [If(True,False,True);False]

let%test "test_trace1_2" = 
  BoolexprLib.Ast.is_value (trace1 (parse "if true then false else true"))

let%test "test_trace1_3" =
  let max_steps = 10 in
  "if (if false then false else false) then (if false then true else false) else (if true then false else true)" 
  |> parse
  |> trace1
  |> trace
  |> List.length
  |> ( > ) max_steps

(* ### Unit tests for task 6 *)

let%test "test_eval_and_or" =
  test_eval "if true && false then true else false || true" true

let%test "test_trace1_and_or" =
  test_trace1 "if true && false then true else true || false" [If(False,True,If(True,True,False));If(True,True,False);True]