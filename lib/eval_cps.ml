open! Environment
open Syntax
open Result

module EvalCPS = struct
  let rec eval expr env cont =
    let binop f e1 e2 env cont =
      eval e2 env (fun v2 ->
          eval e1 env (fun v1 ->
              match (v1, v2) with
              | IntVal n1, IntVal n2 -> cont (IntVal (f n1 n2))
              | _ -> failwith "type error"))
    in
    match expr with
    | Var x -> (
        match Environment.lookup x env with
        | Ok v -> cont v
        | Error e -> error e)
    | IntLit n -> cont (IntVal n)
    | BoolLit b -> cont (BoolVal b)
    | Plus (e1, e2) -> binop ( + ) e1 e2 env cont
    | Times (e1, e2) -> binop ( * ) e1 e2 env cont
    | Eq (e1, e2) ->
        eval e1 env (fun v1 ->
            eval e2 env (fun v2 ->
                match eq_val v1 v2 with
                | Ok b -> cont (BoolVal b)
                | Error e -> error e))
    | If (cond, _then, _else) ->
        eval cond env (function
          | BoolVal true -> eval _then env cont
          | BoolVal false -> eval _else env cont
          | _ -> error "wrong value")
    | Let (x, e1, e2) ->
        eval e1 env (fun v1 ->
            let env1 = Environment.ext env x v1 in
            eval e2 env1 cont)
    | LetRec (f, x, e1, e2) ->
        let env1 = Environment.ext env f (RecFunVal (f, x, e1, env)) in
        eval e2 env1 cont
    | Fun (x, e1) -> cont (FunVal (x, e1, env))
    | App (e1, e2) ->
        eval e1 env (fun funpart ->
            eval e2 env (fun arg -> app funpart arg cont))
    | Empty -> cont (ListVal [])
    | Cons (e1, e2) ->
        eval e1 env (fun v1 ->
            eval e2 env (fun v2 ->
                match v2 with
                | ListVal l -> cont (ListVal (v1 :: l))
                | _ -> error "type error"))
    | Head e ->
        eval e env (function
          | ListVal (h :: _) -> cont h
          | _ -> error "type error")
    | Tail e ->
        eval e env (function
          | ListVal (_ :: t) -> cont (ListVal t)
          | _ -> error "type error")
    | _ -> error "not implemented"

  and eq_val v1 v2 =
    let equal_lists l1 l2 =
      List.length l1 = List.length l2
      && List.for_all2
           (fun v1 v2 -> match eq_val v1 v2 with Ok b -> b | Error _ -> false)
           l1 l2
    in
    match (v1, v2) with
    | IntVal n1, IntVal n2 -> ok (n1 = n2)
    | BoolVal b1, BoolVal b2 -> ok (b1 = b2)
    | ListVal l1, ListVal l2 -> ok (equal_lists l1 l2)
    | _ -> error "type error"

  and app funpart arg cont =
    match funpart with
    | FunVal (x, body, env1) -> eval body (Environment.ext env1 x arg) cont
    | RecFunVal (f, x, body, env1) ->
        let env2 = Environment.ext (Environment.ext env1 x arg) f funpart in
        eval body env2 cont
    | _ -> error "type error"
end
