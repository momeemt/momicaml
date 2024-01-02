module Ast : sig
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

  val exp_abs : exp -> exp
  val string_of_value : value -> string
  val string_of_exp : exp -> string
end

