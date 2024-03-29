open Syntax
open! Environment

module Eval = struct
  let rec equal_vals v1 v2 =
    match (v1, v2) with
    | IntVal n1, IntVal n2 -> n1 = n2
    | BoolVal b1, BoolVal b2 -> b1 = b2
    | ListVal l1, ListVal l2 -> equal_lists l1 l2
    | _, _ -> failwith "type error"

  and equal_lists l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> equal_vals h1 h2 && equal_lists t1 t2
    | _, _ -> false

  let rec eval e env =
    let binop f e1 e2 env =
      let right = eval e2 env in
      let left = eval e1 env in
      match (left, right) with
      | IntVal n1, IntVal n2 -> IntVal (f n1 n2)
      | _, _ -> failwith "type error"
    in
    match e with
    | IntLit n -> IntVal n
    | Plus (e1, e2) -> binop ( + ) e1 e2 env
    | Minus (e1, e2) -> binop ( - ) e1 e2 env
    | Times (e1, e2) -> binop ( * ) e1 e2 env
    | Div (e1, e2) -> binop ( / ) e1 e2 env
    | Eq (e1, e2) ->
        let right = eval e2 env in
        let left = eval e1 env in
        BoolVal (equal_vals left right)
    | Neq (e1, e2) -> (
        let right = eval e2 env in
        let left = eval e1 env in
        match (left, right) with
        | IntVal n1, IntVal n2 -> BoolVal (n1 <> n2)
        | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
        | _, _ -> failwith "type error")
    | Greater (e1, e2) -> (
        let right = eval e2 env in
        let left = eval e1 env in
        match (left, right) with
        | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
        | _, _ -> failwith "type error")
    | BoolLit b -> BoolVal b
    | If (cond, _then, _else) -> (
        match eval cond env with
        | BoolVal true -> eval _then env
        | BoolVal false -> eval _else env
        | _ -> failwith "type error")
    | Var x -> (
        match Environment.lookup x env with
        | Ok v -> v
        | Error _ -> failwith "unbound variable")
    | Let (x, e1, e2) ->
        let env1 = Environment.ext env x (eval e1 env) in
        eval e2 env1
    | LetRec (f, x, e1, e2) ->
        let env1 = Environment.ext env f (RecFunVal (f, x, e1, env)) in
        eval e2 env1
    | Fun (x, e1) -> FunVal (x, e1, env)
    | App (e1, e2) -> (
        let funpart = eval e1 env in
        let arg = eval e2 env in
        match funpart with
        | FunVal (x, body, env1) ->
            let env2 = Environment.ext env1 x arg in
            eval body env2
        | RecFunVal (f, x, body, env1) ->
            let env2 = Environment.ext (Environment.ext env1 x arg) f funpart in
            eval body env2
        | _ -> failwith "function value expected")
    | Empty -> ListVal []
    | Cons (e1, e2) -> (
        let head = eval e1 env in
        let tail = eval e2 env in
        match (head, tail) with
        | v1, ListVal v2 -> ListVal (v1 :: v2)
        | _, _ -> failwith "type error")
    | Head e -> (
        match eval e env with
        | ListVal (h :: _) -> h
        | _ -> failwith "type error")
    | Tail e -> (
        match eval e env with
        | ListVal (_ :: t) -> ListVal t
        | _ -> failwith "type error")
    | _ -> failwith "not implemented"
end
