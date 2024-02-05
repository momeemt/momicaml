open Momicaml.Compiler
open Momicaml.Cam
open Momicaml.Environment
open Momicaml.Typecheck

let test_compiler_let1 () =
  let tenv = Environment.ext (Environment.emptyEnv ()) "x" TInt in
  let stack_res = Compiler.compile "let x = 1 in x + 1" tenv in
  let stack = (
    match stack_res with
    | Ok stack -> stack
    | Error e -> failwith ("Failed to compile " ^ e)
  ) in
  let result = match List.hd stack with
    | CAM.CAM_IntCal i -> i
    | _ -> failwith "Not an int" in
  let expected = 2 in
  Alcotest.(check int) "same result" expected result

let test_compiler_let2 () =
  let tenv = Environment.ext (Environment.ext (Environment.emptyEnv ()) "x" TInt) "y" TInt in
  let stack_res = Compiler.compile "let x = 1 in let y = 2 in x + y" tenv in
  let stack = (
    match stack_res with
    | Ok stack -> stack
    | _ -> failwith "Failed to compile"
  ) in
  let result = match List.hd stack with
    | CAM.CAM_IntCal i -> i
    | _ -> failwith "Not an int" in
  let expected = 3 in
  Alcotest.(check int) "same result" expected result

let test_compiler_if1 () =
  let stack_res = Compiler.compile "if true then 1 else 2" (Environment.emptyEnv()) in
  let stack = (
    match stack_res with
    | Ok stack -> stack
    | _ -> failwith "Failed to compile"
  ) in
  let result = match List.hd stack with
    | CAM.CAM_IntCal i -> i
    | _ -> failwith "Not an int" in
  let expected = 1 in
  Alcotest.(check int) "same result" expected result

let test_compiler_if2 () =
  let stack_res = Compiler.compile "if false then 1 else 2" (Environment.emptyEnv()) in
  let stack = (
    match stack_res with
    | Ok stack -> stack
    | _ -> failwith "Failed to compile"
  ) in
  let result = match List.hd stack with
    | CAM.CAM_IntCal i -> i
    | _ -> failwith "Not an int" in
  let expected = 2 in
  Alcotest.(check int) "same result" expected result

let test_compiler_if_not_pass_typecheck1 () =
  let stack_res = Compiler.compile "if 1 then 1 else 2" (Environment.emptyEnv()) in
  match stack_res with
    | Error e -> Alcotest.(check string) "same result" "type error in If" e
    | _ -> failwith "Compiler should not pass typecheck"

let () =
  Alcotest.run "Momicaml" [
    "test_compiler_let1", [
      Alcotest.test_case "test_compiler_let1" `Quick test_compiler_let1;
    ];
    "test_compiler_let2", [
      Alcotest.test_case "test_compiler_let2" `Quick test_compiler_let2;
    ];
    "test_compiler_if1", [
      Alcotest.test_case "test_compiler_if1" `Quick test_compiler_if1;
    ];
    "test_compiler_if2", [
      Alcotest.test_case "test_compiler_if2" `Quick test_compiler_if2;
    ];
    "test_compiler_if_not_pass_typecheck1", [
      Alcotest.test_case "test_compiler_if_not_pass_typecheck1" `Quick test_compiler_if_not_pass_typecheck1;
    ];
  ]
