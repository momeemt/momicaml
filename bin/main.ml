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

type value =
  | IntVal of int
  | BoolVal of bool

let exp_plus2 e = Plus(e, IntLit 2)

let rec exp_abs e =
  match e with
  | IntLit n -> IntLit (abs n)
  | Plus (e1, e2) -> Plus (exp_abs e1, exp_abs e2)
  | Sub (e1, e2) -> Sub (exp_abs e1, exp_abs e2)
  | Times (e1, e2) -> Times (exp_abs e1, exp_abs e2)
  | Div (e1, e2) -> Div (exp_abs e1, exp_abs e2)

let rec string_of_value v =
  match v with
  | IntVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | _ -> failwith "wrong value"

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

let rec eval2 e =
  match e with
  | IntLit n -> IntVal n
  | Plus (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
        | _ -> failwith "integer values expected"
      end
  | Sub (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
        | _ -> failwith "integer values expected"
      end
  | Times (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
        | _ -> failwith "integer values expected"
      end
  | Div (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 / n2)
        | _ -> failwith "integer values expected"
      end
  | Eq (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> BoolVal(n1=n2)
        | (BoolVal b1, BoolVal b2) -> BoolVal(b1=b2)
        | _ -> failwith "wrong value"
      end
  | Greater (e1, e2) ->
      begin
        match (eval2 e1, eval2 e2) with
        | (IntVal n1, IntVal n2) -> BoolVal(n1 > n2)
        | _ -> failwith "wrong value"
      end
  | BoolLit b -> BoolVal b
  | If (cond, _then, _else) ->
      begin
        match (eval2 cond) with
        | BoolVal true -> eval2 _then
        | BoolVal false -> eval2 _else
        | _ -> failwith "wrong value"
      end
  | _ -> failwith "unknown expression e"

let () =
  let v1 = eval2 (IntLit 1) in
  Printf.printf "%s\n" (string_of_value v1);;

  let v2 = eval2 (IntLit 11) in
  Printf.printf "%s\n" (string_of_value v2);;

  let v3 = eval2 (Plus (IntLit 1, Plus (IntLit 2, IntLit 11))) in
  Printf.printf "%s\n" (string_of_value v3);;

  let v4 = eval2 (Times (IntLit 1, Plus (IntLit 2, IntLit 11))) in
  Printf.printf "%s\n" (string_of_value v4);;

  let v5 = eval2 (If (Eq(IntLit 2, IntLit 11), Times(IntLit 1, IntLit 2), Times(IntLit 1, Plus(IntLit 2,IntLit 3)))) in
  Printf.printf "%s\n" (string_of_value v5);;

  let v6 = eval2 (Eq (IntLit 1, IntLit 1)) in
  Printf.printf "%s\n" (string_of_value v6);;

  let v7 = eval2 (Eq (IntLit 1, IntLit 2)) in
  Printf.printf "%s\n" (string_of_value v7);;

  let v8 = eval2 (Eq (BoolLit true, BoolLit true)) in
  Printf.printf "%s\n" (string_of_value v8);;

  let v9 = eval2 (Eq (BoolLit true, BoolLit false)) in
  Printf.printf "%s\n" (string_of_value v9);;
  
  (* Plusにbool値を与えたので例外が投げられた *)
  try let v_err1 = eval2 (Plus (BoolLit true, BoolLit false)) in () with
  | Failure msg -> Printf.printf "%s\n" msg;;

  (* intとboolを比較したので例外が投げられた *)
  try let v_err2 = eval2 (Eq (BoolLit true, IntLit 1)) in () with
  | Failure msg -> Printf.printf "%s\n" msg;;

  let v10 = eval2 (If (Greater (IntLit 3, IntLit 1), (IntLit 100), (IntLit 200))) in
  Printf.printf "%s\n" (string_of_value v10);;
