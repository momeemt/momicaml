open! Ast
open! Environment

module Eval = struct
  let rec eval e env =
    let binop f e1 e2 env =
      match (eval e1 env, eval e2 env) with
      | Ast.IntVal n1, Ast.IntVal n2 -> Ast.IntVal (f n1 n2)
      | Ast.IntVal _, _ | _, Ast.IntVal _ -> failwith "type error"
      | Ast.BoolVal _, Ast.BoolVal _ -> failwith "type error"
    in
    match e with
    | Ast.IntLit n -> Ast.IntVal n
    | Ast.Plus (e1, e2) -> binop (+) e1 e2 env
    | Ast.Sub (e1, e2) -> binop (-) e1 e2 env
    | Ast.Times (e1, e2) -> binop ( * ) e1 e2 env
    | Ast.Div (e1, e2) -> binop (/) e1 e2 env
    | Ast.Eq (e1, e2) ->
      begin
        match (eval e1 env, eval e2 env) with
        | Ast.IntVal n1, Ast.IntVal n2 -> Ast.BoolVal (n1 = n2)
        | Ast.BoolVal b1, Ast.BoolVal b2 -> Ast.BoolVal (b1 = b2)
        | Ast.IntVal _, Ast.BoolVal _ | Ast.BoolVal _, Ast.IntVal _ -> failwith "type error"
      end
    | Ast.Greater (e1, e2) ->
      begin
        match (eval e1 env, eval e2 env) with
        | Ast.IntVal n1, Ast.IntVal n2 -> Ast.BoolVal (n1 > n2)
        | Ast.IntVal _, Ast.BoolVal _ | Ast.BoolVal _, Ast.IntVal _ -> failwith "type error"
        | Ast.BoolVal _, Ast.BoolVal _ -> failwith "type error"
      end
    | Ast.BoolLit b -> Ast.BoolVal b
    | Ast.If (cond, _then, _else) ->
      begin
        match eval cond env with
        | Ast.BoolVal true -> eval _then env
        | Ast.BoolVal false -> eval _else env
        | Ast.IntVal _ -> failwith "type error"
      end
    | Ast.Var x -> Environment.lookup x env
    | Ast.Let (x, e1, e2) ->
        let env1 = Environment.ext env x (eval e1 env)
        in eval e2 env1
end
