open Momicaml.Syntax
open Momicaml.Typeinfer
open Momicaml.Environment
open Momicaml.Types

let test_typeinfer_successful1 () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match TypeInferer.tinf_old (Environment.emptyEnv ()) expr with
  | Ok (_, t) -> Alcotest.(check string) (ty_to_string t) "int" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_typeinfer_successful2 () =
  let expr = If (BoolLit true, BoolLit false, BoolLit true) in
  match TypeInferer.tinf_old (Environment.emptyEnv ()) expr with
  | Ok (_, t) ->
      Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_typeinfer_failed () =
  let expr = If (BoolLit true, Plus (IntLit 1, BoolLit false), IntLit 100) in
  match TypeInferer.tinf_old (Environment.emptyEnv ()) expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_unify1 () =
  match TypeInferer.unify [ (TVar "'a", TBool) ] with
  | Ok res -> (
      match Environment.lookup "'a" res with
      | Ok t -> Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
      | _ -> Alcotest.fail "unify failed")
  | _ -> Alcotest.fail "unify failed"

let test_unify2 () =
  match TypeInferer.unify [ (TInt, TBool) ] with
  | Error _ -> Alcotest.(check bool) "unify2" true true
  | _ -> Alcotest.fail "unify failed"

let test_unify3 () =
  match TypeInferer.unify [ (TVar "'a", TVar "'b") ] with
  | Ok res -> (
      match Environment.lookup "'a" res with
      | Ok t ->
          Alcotest.(check string) (ty_to_string t) "TVar('b)" (ty_to_string t)
      | _ -> Alcotest.fail "unify failed")
  | _ -> Alcotest.fail "unify failed"

let test_unify4 () =
  match
    TypeInferer.unify
      [ (TArrow (TVar "'a", TVar "'b"), TArrow (TVar "'b", TVar "'c")) ]
  with
  | Ok res -> (
      match (Environment.lookup "'a" res, Environment.lookup "'b" res) with
      | Ok t1, Ok t2 ->
          Alcotest.(check string) (ty_to_string t1) "TVar('c)" (ty_to_string t1);
          Alcotest.(check string) (ty_to_string t2) "TVar('c)" (ty_to_string t2)
      | _ -> Alcotest.fail "unify failed")
  | _ -> Alcotest.fail "unify failed"

let test_unify5 () =
  match TypeInferer.unify [ (TVar "'a", TArrow (TVar "'b", TVar "'a")) ] with
  | Error _ -> Alcotest.(check bool) "unify5" true true
  | _ -> Alcotest.fail "unify failed"

let test_tinf_top1 () =
  let expr = Fun ("f", Fun ("x", App (Var "f", App (Var "f", Var "x")))) in
  match TypeInferer.tinf_top expr with
  | Ok _ -> Alcotest.(check bool) "tinf_top1" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top2 () =
  let expr =
    Fun
      ( "x",
        Fun
          ("y", Fun ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))
      )
  in
  match TypeInferer.tinf_top expr with
  | Ok _ -> Alcotest.(check bool) "tinf_top2" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_1 () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match TypeInferer.tinf_top expr with
  | Ok _ -> Alcotest.(check bool) "tinf_top_if_1" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_2 () =
  let expr = If (BoolLit true, BoolLit false, BoolLit true) in
  match TypeInferer.tinf_top expr with
  | Ok _ -> Alcotest.(check bool) "tinf_top_if_2" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_3 () =
  let expr = If (BoolLit true, Fun ("x", Var "x"), Fun ("x", Var "x")) in
  match TypeInferer.tinf_top expr with
  | Ok _ -> Alcotest.(check bool) "tinf_top_if_3" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_4 () =
  let expr = If (BoolLit true, Fun ("x", Var "x"), IntLit 1) in
  match TypeInferer.tinf_top expr with
  | Error _ -> Alcotest.(check bool) "tinf_top_if_4" true true
  | _ -> Alcotest.fail "typeinfer failed"

let () =
  Alcotest.run "Momicaml.TypeInferer"
    [
      ( "tinf_old",
        [
          Alcotest.test_case "test_typeinfer_successful1" `Quick
            test_typeinfer_successful1;
          Alcotest.test_case "test_typeinfer_successful2" `Quick
            test_typeinfer_successful2;
          Alcotest.test_case "test_typeinfer_failed" `Quick
            test_typeinfer_failed;
        ] );
      ( "unify",
        [
          Alcotest.test_case "test_unify1" `Quick test_unify1;
          Alcotest.test_case "test_unify2" `Quick test_unify2;
          Alcotest.test_case "test_unify3" `Quick test_unify3;
          Alcotest.test_case "test_unify4" `Quick test_unify4;
          Alcotest.test_case "test_unify5" `Quick test_unify5;
        ] );
      ( "tinf_top",
        [
          Alcotest.test_case "test_tinf_top1" `Quick test_tinf_top1;
          Alcotest.test_case "test_tinf_top2" `Quick test_tinf_top2;
          Alcotest.test_case "test_tinf_top_if_1" `Quick test_tinf_top_if_1;
          Alcotest.test_case "test_tinf_top_if_2" `Quick test_tinf_top_if_2;
          Alcotest.test_case "test_tinf_top_if_3" `Quick test_tinf_top_if_3;
          Alcotest.test_case "test_tinf_top_if_4" `Quick test_tinf_top_if_4;
        ] );
    ]
