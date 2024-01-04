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

and env = (string * value) list
