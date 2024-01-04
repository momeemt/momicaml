open Momicaml.Environment
open Momicaml.Syntax

let test_lookup_single_item_environment () =
  let actual = Environment.lookup "x" (Environment.ext (Environment.emptyEnv()) "x" (IntVal 1)) in
  match actual with
  | IntVal n -> Alcotest.(check int) "lookup_single" 1 n
  | _ -> Alcotest.fail "Expected IntVal, got something else"

let test_lookup_multiple_items_environment () =
  let env = Environment.ext (Environment.ext (Environment.ext (Environment.emptyEnv()) "x" (IntVal 1)) "y" (BoolVal true)) "z" (IntVal 5) in
  let actual_x = Environment.lookup "x" env in
  let actual_y = Environment.lookup "y" env in
  let actual_z = Environment.lookup "z" env in
  let () =
    match actual_x with
    | IntVal n -> Alcotest.(check int) "lookup_multiple" 1 n
    | _ -> Alcotest.fail "Expected IntVal, got something else"
  in
  let () =
    match actual_y with
    | BoolVal b -> Alcotest.(check bool) "lookup_multiple" true b
    | _ -> Alcotest.fail "Expected BoolVal, got something else"
  in
  let () =
    match actual_z with
    | IntVal n -> Alcotest.(check int) "lookup_multiple" 5 n
    | _ -> Alcotest.fail "Expected IntVal, got something else"
  in
  Alcotest.check_raises "lookup nonexistence variable" (Failure "unbound variable: w") (fun () ->
    let _ = Environment.lookup "w" env
    in ()
  )

let test_lookup_duplicate_items_environment () =
  let env = Environment.ext (Environment.ext (Environment.ext (Environment.emptyEnv()) "x" (IntVal 1)) "y" (BoolVal true)) "x" (IntVal 999) in
  let actual_x = Environment.lookup "x" env in
  match actual_x with
  | IntVal n -> Alcotest.(check int) "lookup_duplicate" 999 n
  | _ -> Alcotest.fail "Expected IntVal, got something else"

let () =
  Alcotest.run "Momicaml.environment" [
    ("Environment.lookup", [
        Alcotest.test_case "lookup_single" `Quick test_lookup_single_item_environment;
        Alcotest.test_case "lookup_multiple" `Quick test_lookup_multiple_items_environment;
        Alcotest.test_case "lookup_duplicate" `Quick test_lookup_duplicate_items_environment;
      ]);
  ]
