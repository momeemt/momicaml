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
        ] );
    ]
