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
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t) "TVar('a3) -> TVar('a3) -> TVar('a3) -> TVar('a3)"
        (ty_to_string t)
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
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t)
        "TVar('a2) -> TVar('a4) -> TVar('a5) -> TVar('a2) -> TVar('a4) -> \
         TVar('a2) -> TVar('a5)"
        (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_1 () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "int" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_2 () =
  let expr = If (BoolLit true, BoolLit false, BoolLit true) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_3 () =
  let expr = If (BoolLit true, Fun ("x", Var "x"), Fun ("x", Var "x")) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t) "TVar('a0) -> TVar('a0)" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_if_4 () =
  let expr = If (BoolLit true, Fun ("x", Var "x"), IntLit 1) in
  match TypeInferer.tinf_top expr with
  | Error _ -> Alcotest.(check bool) "tinf_top_if_4" true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_let1 () =
  let expr = Let ("x", IntLit 1, Var "x") in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "int" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_let2 () =
  let expr = Let ("x", Fun ("x", Var "x"), Var "x") in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t) "TVar('a0) -> TVar('a0)" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_letrec1 () =
  let expr = LetRec ("f", "x", App (Var "f", Var "x"), Var "f") in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t) "TVar('a0) -> TVar('a2)" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_list () =
  let expr = Let ("x", Empty, Var "x") in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string)
        (ty_to_string t) "List(TVar('a0))" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_list_cons () =
  let expr = Let ("x", Cons (IntLit 1, Empty), Var "x") in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "List(int)" (ty_to_string t)
  | Error e -> Alcotest.fail e

let test_tinf_top_list_head () =
  let expr = Let ("x", Cons (IntLit 1, Empty), Head (Var "x")) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "int" (ty_to_string t)
  | Error e -> Alcotest.fail e

let test_tinf_top_list_tail () =
  let expr = Let ("x", Cons (IntLit 1, Empty), Tail (Var "x")) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "List(int)" (ty_to_string t)
  | Error e -> Alcotest.fail e

let test_tinf_top_eq () =
  let expr = Let ("x", IntLit 1, Eq (Var "x", IntLit 1)) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
  | Error e -> Alcotest.fail e

let test_tinf_top_eq_error () =
  let expr = Let ("x", IntLit 1, Eq (Var "x", BoolLit true)) in
  match TypeInferer.tinf_top expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typeinfer failed"

let test_tinf_top_neq () =
  let expr = Let ("x", IntLit 1, Neq (Var "x", IntLit 1)) in
  match TypeInferer.tinf_top expr with
  | Ok (_, t, _, _) ->
      Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
  | Error e -> Alcotest.fail e

let test_tinf_top_neq_error () =
  let expr = Let ("x", IntLit 1, Neq (Var "x", BoolLit true)) in
  match TypeInferer.tinf_top expr with
  | Error e -> Alcotest.(check bool) e true true
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
          Alcotest.test_case "test_tinf_top_let1" `Quick test_tinf_top_let1;
          Alcotest.test_case "test_tinf_top_let2" `Quick test_tinf_top_let2;
          Alcotest.test_case "test_tinf_top_letrec1" `Quick
            test_tinf_top_letrec1;
          Alcotest.test_case "test_tinf_top_list" `Quick test_tinf_top_list;
          Alcotest.test_case "test_tinf_top_list_cons" `Quick
            test_tinf_top_list_cons;
          Alcotest.test_case "test_tinf_top_list_head" `Quick
            test_tinf_top_list_head;
          Alcotest.test_case "test_tinf_top_list_tail" `Quick
            test_tinf_top_list_tail;
          Alcotest.test_case "test_tinf_top_eq" `Quick test_tinf_top_eq;
          Alcotest.test_case "test_tinf_top_eq_error" `Quick
            test_tinf_top_eq_error;
          Alcotest.test_case "test_tinf_top_neq" `Quick test_tinf_top_neq;
          Alcotest.test_case "test_tinf_top_neq_error" `Quick
            test_tinf_top_neq_error;
        ] );
    ]
