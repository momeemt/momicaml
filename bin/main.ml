type exp =
  | IntLit of int
  | Plus of exp * exp
  | Sub of exp * exp
  | Times of exp * exp
  | Div of exp * exp

let exp_plus2 e = Plus(e, IntLit 2)

let rec exp_abs e =
  match e with
  | IntLit n -> IntLit (abs n)
  | Plus (e1, e2) -> Plus (exp_abs e1, exp_abs e2)
  | Sub (e1, e2) -> Sub (exp_abs e1, exp_abs e2)
  | Times (e1, e2) -> Times (exp_abs e1, exp_abs e2)
  | Div (e1, e2) -> Div (exp_abs e1, exp_abs e2)

let rec string_of_exp e =
  match e with
  | IntLit n -> string_of_int n
  | Plus (e1, e2) -> "(" ^ (string_of_exp e1) ^ " + " ^ (string_of_exp e2) ^ ")"
  | Sub (e1, e2) -> "(" ^ (string_of_exp e1) ^ " - " ^ (string_of_exp e2) ^ ")"
  | Times (e1, e2) -> "(" ^ (string_of_exp e1) ^ " * " ^ (string_of_exp e2) ^ ")"
  | Div (e1, e2) -> "(" ^ (string_of_exp e1) ^ " / " ^ (string_of_exp e2) ^ ")"

let rec eval1 e =
  match e with
  | IntLit n -> n
  | Plus (e1, e2) -> eval1 e1 + (eval1 e2)
  | Sub (e1, e2) -> eval1 e1 - (eval1 e2)
  | Times (e1, e2) -> eval1 e1 * (eval1 e2)
  | Div (e1, e2) -> if e2 = IntLit 0 then failwith "zero devided error" else eval1 e1 / (eval1 e2)
  | _ -> failwith "unknown expression"

let () =
  let e1 = eval1 (Plus (IntLit 1, (Times (IntLit 2, IntLit 3)))) in
  Printf.printf "%d\n" e1;;

  let e2 = eval1 (Plus(Times(Times (IntLit 4, IntLit 1), IntLit 10), IntLit 5)) in
  Printf.printf "%d\n" e2;;

  let e3 = eval1 (Div(Plus(Sub(IntLit 12, IntLit 9), Times(IntLit 23, IntLit 2)), IntLit 7)) in
  Printf.printf "%d\n" e3;;

  let e4 = eval1 (Div(IntLit 3, IntLit 0)) in
  Printf.printf "%d\n" e4;;
