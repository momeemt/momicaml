type exp =
  | Var of string
  | IntLit of int
  | BoolLit of bool
  | If of exp * exp * exp
  | Let of string * exp * exp
  | LetRec of string * string * exp * exp
  | Fun of string * exp
  | App of exp * exp
  | Eq of exp * exp
  | Neq of exp * exp
  | Greater of exp * exp
  | Less of exp * exp
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | Empty
  | Match of exp * (exp * exp) list
  | Cons of exp * exp
  | Head of exp
  | Tail of exp

type value =
  | IntVal of int
  | BoolVal of bool
  | ListVal of value list
  | FunVal of string * exp * env
  | RecFunVal of string * string * exp * env
  | RecFunVal2 of string * string * exp * env ref

and env = (string, value) Hashtbl.t

let rec string_of_match lst = match lst with
  | [] -> ""
  | (e1, e2)::[] -> string_of_expr e1 ^ " -> " ^ string_of_expr e2
  | (e1, e2)::t -> string_of_expr e1 ^ " -> " ^ string_of_expr e2 ^ " | " ^ string_of_match t
and string_of_expr expr = match expr with
  | Var x -> x
  | IntLit n -> string_of_int n
  | BoolLit b -> string_of_bool b
  | If (e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | LetRec (f, x, e1, e2) -> "let rec " ^ f ^ " " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ string_of_expr e ^ ")"
  | App (e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
  | Eq (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Neq (e1, e2) -> string_of_expr e1 ^ " != " ^ string_of_expr e2
  | Greater (e1, e2) -> string_of_expr e1 ^ " > " ^ string_of_expr e2
  | Less (e1, e2) -> string_of_expr e1 ^ " < " ^ string_of_expr e2
  | Plus (e1, e2) -> string_of_expr e1 ^ " + " ^ string_of_expr e2
  | Minus (e1, e2) -> string_of_expr e1 ^ " - " ^ string_of_expr e2
  | Times (e1, e2) -> string_of_expr e1 ^ " * " ^ string_of_expr e2
  | Div (e1, e2) -> string_of_expr e1 ^ " / " ^ string_of_expr e2
  | Empty -> "[]"
  | Match (e, lst) -> "match " ^ string_of_expr e ^ " with " ^ string_of_match lst
  | Cons (e1, e2) -> string_of_expr e1 ^ " :: " ^ string_of_expr e2
  | Head e -> "head " ^ string_of_expr e
  | Tail e -> "tail " ^ string_of_expr e