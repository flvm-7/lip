type ast =
  | Const of int
  | Hex of string
  | UnMinus of ast
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast

 