open Momicaml.Syntax
open Momicaml.Typeinfer
open Momicaml.Environment
open Momicaml.Types

let test_typeinfer_successful1 () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match TypeInferer.tinf (Environment.emptyEnv ()) expr with
  | Ok (_, t) -> Alcotest.(check string) (ty_to_string t) "int" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_typeinfer_successful2 () =
  let expr = If (BoolLit true, BoolLit false, BoolLit true) in
  match TypeInferer.tinf (Environment.emptyEnv ()) expr with
  | Ok (_, t) ->
      Alcotest.(check string) (ty_to_string t) "bool" (ty_to_string t)
  | _ -> Alcotest.fail "typeinfer failed"

let test_typeinfer_failed () =
  let expr = If (BoolLit true, Plus (IntLit 1, BoolLit false), IntLit 100) in
  match TypeInferer.tinf (Environment.emptyEnv ()) expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typeinfer failed"

let () =
  Alcotest.run "Momicaml.TypeInferer"
    [
      ( "tinf",
        [
          Alcotest.test_case "test_typeinfer_successful1" `Quick
            test_typeinfer_successful1;
          Alcotest.test_case "test_typeinfer_successful2" `Quick
            test_typeinfer_successful2;
          Alcotest.test_case "test_typeinfer_failed" `Quick
            test_typeinfer_failed;
        ] );
    ]
