open Momicaml.Syntax
open Momicaml.Typecheck

let test_typecheck_successful () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match tcheck expr with
  | Ok TInt -> Alcotest.(check bool) "typecheck_successful" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_failed () =
  let expr = If (BoolLit true, Plus (IntLit 1, BoolLit false), IntLit 100) in
  match tcheck expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_int () =
  let expr = Eq (IntLit 1, IntLit 2) in
  match tcheck expr with
  | Ok TBool -> Alcotest.(check bool) "typecheck_eq_int" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_bool () =
  let expr = Eq (BoolLit true, BoolLit false) in
  match tcheck expr with
  | Ok TBool -> Alcotest.(check bool) "typecheck_eq_bool" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_failed () =
  let expr = Eq (IntLit 0, BoolLit false) in
  match tcheck expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let () =
  Alcotest.run "Momicaml.Typecheck"
    [
      ( "tcheck",
        [
          Alcotest.test_case "test_typecheck_successful" `Quick
            test_typecheck_successful;
          Alcotest.test_case "test_typecheck_failed" `Quick
            test_typecheck_failed;
          Alcotest.test_case "test_typecheck_eq_int" `Quick
            test_typecheck_eq_int;
          Alcotest.test_case "test_typecheck_eq_bool" `Quick
            test_typecheck_eq_bool;
          Alcotest.test_case "test_typecheck_eq_failed" `Quick
            test_typecheck_failed;
        ] );
    ]
