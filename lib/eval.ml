open Syntax
open! Environment

module Eval = struct
  let rec eval e env =
    let binop f e1 e2 env =
      match (eval e1 env, eval e2 env) with
      | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
      | _, _ -> failwith "type error"
    in
    match e with
    | IntLit n -> IntVal n
    | Plus (e1, e2) -> binop ( + ) e1 e2 env
    | Minus (e1, e2) -> binop ( - ) e1 e2 env
    | Times (e1, e2) -> binop ( * ) e1 e2 env
    | Div (e1, e2) -> binop ( / ) e1 e2 env
    | Eq (e1, e2) -> (
        match (eval e1 env, eval e2 env) with
        | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
        | _, _ -> failwith "type error")
    | Greater (e1, e2) -> (
        match (eval e1 env, eval e2 env) with
        | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
        | _, _ -> failwith "type error")
    | BoolLit b -> BoolVal b
    | If (cond, _then, _else) -> (
        match eval cond env with
        | BoolVal true -> eval _then env
        | BoolVal false -> eval _else env
        | _ -> failwith "type error")
    | Var x -> Environment.lookup x env
    | Let (x, e1, e2) ->
        let env1 = Environment.ext env x (eval e1 env) in
        eval e2 env1
    | _ -> failwith "not implemented"
end
