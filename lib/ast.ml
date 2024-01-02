[@@@warning "-37"]
module Ast = struct
  type exp =
    | IntLit of int
    | Plus of exp * exp
    | Sub of exp * exp
    | Times of exp * exp
    | Div of exp * exp
    | BoolLit of bool
    | If of exp * exp * exp
    | Eq of exp * exp
    | Greater of exp * exp
    | Var of string
    | Let of string * exp * exp

  type value =
    | IntVal of int
    | BoolVal of bool

  let rec exp_abs e =
    match e with
    | IntLit n -> IntLit (abs n)
    | Plus (e1, e2) -> Plus (exp_abs e1, exp_abs e2)
    | Sub (e1, e2) -> Sub (exp_abs e1, exp_abs e2)
    | Times (e1, e2) -> Times (exp_abs e1, exp_abs e2)
    | Div (e1, e2) -> Div (exp_abs e1, exp_abs e2)
    | If (_, _, _) | Eq (_, _) | Greater (_, _) | BoolLit _ | Var _ | Let (_, _, _) -> failwith "type error"

  let string_of_value v =
    match v with
    | IntVal n -> string_of_int n
    | BoolVal b -> string_of_bool b

  let rec string_of_exp e =
    match e with
    | IntLit n -> string_of_int n
    | Plus (e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Sub (e1, e2) -> "(" ^ string_of_exp e1 ^ " - " ^ string_of_exp e2 ^ ")"
    | Times (e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"
    | Div (e1, e2) -> "(" ^ string_of_exp e1 ^ " / " ^ string_of_exp e2 ^ ")"
    | BoolLit b -> string_of_bool b
    | If (e1, e2, e3) ->
        "(if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2 ^ " else "
        ^ string_of_exp e3 ^ ")"
    | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"
    | Greater (e1, e2) ->
        "(" ^ string_of_exp e1 ^ " > " ^ string_of_exp e2 ^ ")"
    | Var x -> x
    | Let (x, e1, e2) ->
        "(let " ^ x ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
end

