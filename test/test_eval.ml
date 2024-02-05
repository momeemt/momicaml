open Momicaml.Syntax
open! Momicaml.Environment
open! Momicaml.Eval

let test_eval_expr () =
  let env = Environment.ext (Environment.emptyEnv ()) "x" (IntVal 1) in
  let expr = Var "x" in
  let result = Eval.eval expr env in
  match result with
  | IntVal i -> Alcotest.(check int) "eval expr" 1 i
  | _ -> Alcotest.fail "eval expr"

let test_eval_nest_expr1 () =
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "x" (IntVal 1))
      "x" (IntVal 2)
  in
  let expr = Var "x" in
  let result = Eval.eval expr env in
  match result with
  | IntVal i -> Alcotest.(check int) "eval expr" 2 i
  | _ -> Alcotest.fail "eval expr"

let test_eval_nest_expr2 () =
  let env = Environment.emptyEnv () in
  let expr =
    Let
      ( "x",
        IntLit 1,
        Let ("y", Plus (Var "x", IntLit 1), Plus (Var "x", Var "y")) )
  in
  let result = Eval.eval expr env in
  match result with
  | IntVal i -> Alcotest.(check int) "eval expr" 3 i
  | _ -> Alcotest.fail "eval expr"

let test_eval_unbundling_expr () =
  let env = Environment.emptyEnv () in
  let expr =
    Let
      ( "x",
        IntLit 1,
        Plus
          ( Let ("x", IntLit 2, Plus (Var "x", IntLit 1)),
            Times (Var "x", IntLit 2) ) )
  in
  let result = Eval.eval expr env in
  match result with
  | IntVal i -> Alcotest.(check int) "eval expr" 5 i
  | _ -> Alcotest.fail "eval expr"

let test_eval_list_cons () =
  let env = Environment.emptyEnv () in
  let expr = Cons (IntLit 1, Empty) in
  let result = Eval.eval expr env in
  match result with
  | ListVal lst -> (
    match List.hd lst with
    | IntVal i -> Alcotest.(check int) "eval list cons" 1 i
    | _ -> Alcotest.fail "eval list cons"
  )
  | _ -> Alcotest.fail "eval list cons"

let test_eval_list_head () =
  let env = Environment.emptyEnv () in
  let expr = Head (Cons (IntLit 1, Empty)) in
  let result = Eval.eval expr env in
  match result with
  | IntVal i -> Alcotest.(check int) "eval list head" 1 i
  | _ -> Alcotest.fail "eval list head"

let test_eval_list_tail () =
  let env = Environment.emptyEnv () in
  let expr = Tail (Cons (IntLit 1, Empty)) in
  let result = Eval.eval expr env in
  match result with
  | ListVal lst -> Alcotest.(check bool) "eval list tail" true (lst = [])
  | _ -> Alcotest.fail "eval list tail"

let test_eval_nest_int_list () =
  let env = Environment.emptyEnv () in
  let ls1 = Cons (Cons (IntLit 1, Cons (IntLit 2, Empty)), Empty) in
  let ls2 = Cons (IntLit 3, Cons (IntLit 4, Empty)) in
  let expr = Cons (ls2, ls1) in
  let result = Eval.eval expr env in
  match result with
  | ListVal lst -> (
    (match List.hd lst with
    | ListVal lst -> (
      match List.hd lst with
      | IntVal i -> Alcotest.(check int) "eval nest int list" 3 i
      | _ -> Alcotest.fail "eval nest int list"
    )
    | _ -> Alcotest.fail "eval nest int list");
    match List.hd (List.tl lst) with
    | ListVal lst -> (
      match List.hd lst with
      | IntVal i -> Alcotest.(check int) "eval nest int list" 1 i
      | _ -> Alcotest.fail "eval nest int list"
    )
    | _ -> Alcotest.fail "eval nest int list"
  )
  | _ -> Alcotest.fail "eval nest int list"

let test_eval_eq_for_list1 () =
  let env = Environment.emptyEnv () in
  let expr = Eq (Cons (IntLit 1, Empty), Cons (IntLit 1, Empty)) in
  let result = Eval.eval expr env in
  match result with
  | BoolVal b -> Alcotest.(check bool) "eval eq for list" true b
  | _ -> Alcotest.fail "eval eq for list"

let test_eval_eq_for_list2 () =
  let env = Environment.emptyEnv () in
  let expr = Eq (Cons (IntLit 1, Empty), Cons (IntLit 2, Empty)) in
  let result = Eval.eval expr env in
  match result with
  | BoolVal b -> Alcotest.(check bool) "eval eq for list" false b
  | _ -> Alcotest.fail "eval eq for list"

let () =
  Alcotest.run "Momicaml.eval"
    [
      ( "Eval.eval",
        [
          Alcotest.test_case "eval expr" `Quick test_eval_expr;
          Alcotest.test_case "eval nest expr1" `Quick test_eval_nest_expr1;
          Alcotest.test_case "eval nest expr2" `Quick test_eval_nest_expr2;
          Alcotest.test_case "eval unbundling expr" `Quick
            test_eval_unbundling_expr;
          Alcotest.test_case "eval list cons" `Quick test_eval_list_cons;
          Alcotest.test_case "eval list head" `Quick test_eval_list_head;
          Alcotest.test_case "eval list tail" `Quick test_eval_list_tail;
          Alcotest.test_case "eval nest int list" `Quick test_eval_nest_int_list;
          Alcotest.test_case "eval eq for list1" `Quick test_eval_eq_for_list1;
          Alcotest.test_case "eval eq for list2" `Quick test_eval_eq_for_list2;
        ] );
    ]
