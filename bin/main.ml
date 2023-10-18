type exp =
  | IntLit of int
  | Plus of exp * exp
  | Sub of exp * exp
  | Times of exp * exp
  | Div of exp * exp

let exp_plus2 e = Plus(e, IntLit 2)

let rec exp_abs e =
  match e with
    IntLit n -> IntLit (abs n)
  | Plus (e1, e2) -> Plus (exp_abs e1, exp_abs e2)
  | Sub (e1, e2) -> Sub (exp_abs e1, exp_abs e2)
  | Times (e1, e2) -> Times (exp_abs e1, exp_abs e2)
  | Div (e1, e2) -> Div (exp_abs e1, exp_abs e2)

let rec string_of_exp e =
  match e with
    IntLit n -> string_of_int n
  | Plus (e1, e2) -> "(" ^ (string_of_exp e1) ^ " + " ^ (string_of_exp e2) ^ ")"
  | Sub (e1, e2) -> "(" ^ (string_of_exp e1) ^ " - " ^ (string_of_exp e2) ^ ")"
  | Times (e1, e2) -> "(" ^ (string_of_exp e1) ^ " * " ^ (string_of_exp e2) ^ ")"
  | Div (e1, e2) -> "(" ^ (string_of_exp e1) ^ " / " ^ (string_of_exp e2) ^ ")"

let () =
  let e1 = IntLit 3 in
  Printf.printf "%s\n" (string_of_exp (exp_plus2 e1));

  let e2 = Plus (IntLit (-2), Times(IntLit 3, IntLit (-4))) in
  Printf.printf "%s\n" (string_of_exp (exp_abs e2));
