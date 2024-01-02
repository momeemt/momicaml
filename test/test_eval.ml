open! Momicaml.Ast
open! Momicaml.Environment
open! Momicaml.Eval

let test_eval_expr () =
  let env = Environment.ext (Environment.emptyEnv()) "x" (Ast.IntVal 1) in
  let expr = Ast.Var "x" in
  let result = Eval.eval expr env in
  match result with
  | Ast.IntVal i -> Alcotest.(check int) "eval expr" 1 i 
  | Ast.BoolVal _ -> Alcotest.fail "eval expr"

let test_eval_nest_expr1 () =
  let env = Environment.ext (Environment.ext (Environment.emptyEnv()) "x" (Ast.IntVal 1)) "x" (Ast.IntVal 2) in
  let expr = Ast.Var "x" in
  let result = Eval.eval expr env in
  match result with
  | Ast.IntVal i -> Alcotest.(check int) "eval expr" 2 i
  | Ast.BoolVal _ -> Alcotest.fail "eval expr"

let test_eval_nest_expr2 () =
  let env = Environment.emptyEnv() in
  let expr = Ast.Let ("x", Ast.IntLit 1, Ast.Let ("y", Ast.Plus (Ast.Var "x", Ast.IntLit 1), Ast.Plus (Ast.Var "x", Ast.Var "y"))) in
  let result = Eval.eval expr env in
  match result with
  | Ast.IntVal i -> Alcotest.(check int) "eval expr" 3 i
  | Ast.BoolVal _ -> Alcotest.fail "eval expr"

let test_eval_unbundling_expr () =
  let env = Environment.emptyEnv() in
  let expr = Ast.Let ("x", Ast.IntLit 1, Ast.Plus (Ast.Let ("x", Ast.IntLit 2, Ast.Plus (Ast.Var "x", Ast.IntLit 1)), Ast.Times (Ast.Var "x", Ast.IntLit 2))) in
  let result = Eval.eval expr env in
  match result with
  | Ast.IntVal i -> Alcotest.(check int) "eval expr" 5 i
  | Ast.BoolVal _ -> Alcotest.fail "eval expr"

let () =
  Alcotest.run "Momicaml.eval" [
    ("Eval.eval", [
      Alcotest.test_case "eval expr" `Quick test_eval_expr;
      Alcotest.test_case "eval nest expr1" `Quick test_eval_nest_expr1;
      Alcotest.test_case "eval nest expr2" `Quick test_eval_nest_expr2;
      Alcotest.test_case "eval unbundling expr" `Quick test_eval_unbundling_expr;
    ])
  ]

