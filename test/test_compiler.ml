open Momicaml.Compiler

let test_compiler_let1 () =
  let expected = "2" in
  match Compiler.compile "let x = 1 in x + 1" with
  | Ok res -> Alcotest.(check string) "same result" expected res
  | _ -> failwith "Failed to compile"

let test_compiler_let2 () =
  let expected = "3" in
  match Compiler.compile "let x = 1 in let y = 2 in x + y" with
  | Ok res -> Alcotest.(check string) "same result" expected res
  | _ -> failwith "Failed to compile"

let test_compiler_if1 () =
  let expected = "1" in
  match Compiler.compile "if true then 1 else 2" with
  | Ok res -> Alcotest.(check string) "same result" expected res
  | _ -> failwith "Failed to compile"

let test_compiler_if2 () =
  let expected = "2" in
  match Compiler.compile "if false then 1 else 2" with
  | Ok res -> Alcotest.(check string) "same result" expected res
  | _ -> failwith "Failed to compile"

(* type error *)
let test_compiler_if_not_pass_typecheck1 () =
  match Compiler.compile "if 1 then 1 else 2" with
  | Error e -> Alcotest.(check string) "same result" "unification failed" e
  | _ -> failwith "Failed to compile"

(* run-time error *)
let test_runtime_error () =
  match Compiler.compile "1 / 0" with
  | Error e -> Alcotest.(check string) "same result" "Division by zero" e
  | _ -> failwith "Failed to compile"

(* syntax error *)
let test_syntax_error () =
  try
    Compiler.compile "1 + " |> ignore;
    Alcotest.fail "should raise syntax error"
  with
  | _ -> Alcotest.(check string) "same result" "Syntax error" "Syntax error"

let test_fact () =
  match Compiler.compile "let rec fact n = if (n = 0) then 1 else (n * fact (n - 1)) in fact 5" with
  | Error e -> Alcotest.(check string) "same result" "Typecheck Error" e
  | _ -> Alcotest.fail "should raise syntax error"

let () =
  Alcotest.run "Momicaml"
    [
      ( "test_compiler_let1",
        [ Alcotest.test_case "test_compiler_let1" `Quick test_compiler_let1 ] );
      ( "test_compiler_let2",
        [ Alcotest.test_case "test_compiler_let2" `Quick test_compiler_let2 ] );
      ( "test_compiler_if1",
        [ Alcotest.test_case "test_compiler_if1" `Quick test_compiler_if1 ] );
      ( "test_compiler_if2",
        [ Alcotest.test_case "test_compiler_if2" `Quick test_compiler_if2 ] );
      ( "test_compiler_if_not_pass_typecheck1",
        [
          Alcotest.test_case "test_compiler_if_not_pass_typecheck1" `Quick
            test_compiler_if_not_pass_typecheck1;
        ] );
      ( "test_runtime_error",
        [ Alcotest.test_case "test_runtime_error" `Quick test_runtime_error ] );
      ( "test_syntax_error",
        [ Alcotest.test_case "test_syntax_error" `Quick test_syntax_error ] );
      ( "test_fact", [ Alcotest.test_case "test_fact" `Quick test_fact ] );
    ]
