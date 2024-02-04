open Momicaml.Syntax
open Momicaml.Typecheck
open Momicaml.Environment

let rec type_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) ->
      Printf.sprintf "(%s -> %s)" (type_to_string t1) (type_to_string t2)

let test_typecheck_successful () =
  let expr = If (BoolLit true, IntLit 1, IntLit 100) in
  match tcheck (Environment.emptyEnv ()) expr with
  | Ok TInt -> Alcotest.(check bool) "typecheck_successful" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_failed () =
  let expr = If (BoolLit true, Plus (IntLit 1, BoolLit false), IntLit 100) in
  match tcheck (Environment.emptyEnv ()) expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_int () =
  let expr = Eq (IntLit 1, IntLit 2) in
  match tcheck (Environment.emptyEnv ()) expr with
  | Ok TBool -> Alcotest.(check bool) "typecheck_eq_int" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_bool () =
  let expr = Eq (BoolLit true, BoolLit false) in
  match tcheck (Environment.emptyEnv ()) expr with
  | Ok TBool -> Alcotest.(check bool) "typecheck_eq_bool" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_eq_failed () =
  let expr = Eq (IntLit 0, BoolLit false) in
  match tcheck (Environment.emptyEnv ()) expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_env1 () =
  let expr = If (BoolLit true, Var "x", IntLit 100) in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "y" TInt)
      "x" TInt
  in
  match tcheck env expr with
  | Ok TInt -> Alcotest.(check bool) "typecheck_env1" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_env2 () =
  let expr = If (BoolLit true, Var "x", IntLit 100) in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "y" TInt)
      "x" TBool
  in
  match tcheck env expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_env3 () =
  let expr = If (BoolLit true, Var "x", IntLit 100) in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "y" TInt)
      "z" TInt
  in
  match tcheck env expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_env4 () =
  let expr = If (Var "x", Var "x", IntLit 100) in
  let env = Environment.ext (Environment.emptyEnv ()) "x" TBool in
  match tcheck env expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_env5 () =
  let expr = If (Var "x", Var "x", IntLit 100) in
  let env = Environment.ext (Environment.emptyEnv ()) "x" TInt in
  match tcheck env expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_fn1 () =
  let expr = Fun ("x", If (BoolLit true, Var "x", IntLit 100)) in
  let env = Environment.ext (Environment.emptyEnv ()) "x" TInt in
  match tcheck env expr with
  | Ok (TArrow (TInt, TInt)) -> Alcotest.(check bool) "typecheck_fn1" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_fn2 () =
  let expr = Fun ("x", If (BoolLit true, Var "x", IntLit 100)) in
  let env = Environment.ext (Environment.emptyEnv ()) "x" TBool in
  match tcheck env expr with
  | Error e -> Alcotest.(check bool) e true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_fn3 () =
  let expr =
    App
      ( Fun ("x", If (BoolLit true, Var "x", IntLit 100)),
        If (BoolLit true, Var "y", IntLit 200) )
  in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "x" TInt)
      "y" TInt
  in
  match tcheck env expr with
  | Ok TInt -> Alcotest.(check bool) "typecheck_fn3" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_fn4 () =
  let expr =
    Fun
      ( "f",
        Fun
          ( "x",
            App
              (Var "f", App (Var "f", App (Var "f", Plus (Var "x", IntLit 10))))
          ) )
  in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "x" TInt)
      "f"
      (TArrow (TInt, TInt))
  in
  match tcheck env expr with
  | Ok (TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))) ->
      Alcotest.(check bool) "typecheck_fn4" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_fn5 () =
  let expr =
    Fun ("f", Fun ("g", Fun ("x", App (Var "f", App (Var "g", Var "x")))))
  in
  let env =
    Environment.ext
      (Environment.ext
         (Environment.ext (Environment.emptyEnv ()) "x" TInt)
         "g"
         (TArrow (TInt, TInt)))
      "f"
      (TArrow (TInt, TInt))
  in
  match tcheck env expr with
  | Ok
      (TArrow
        (TArrow (TInt, TInt), TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt))))
    ->
      Alcotest.(check bool) "typecheck_fn5" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_if_other_than_bool_and_int () =
  let expr =
    If
      ( BoolLit true,
        Fun ("x", Plus (Var "x", IntLit 1)),
        Fun ("y", Times (Var "y", IntLit 2)) )
  in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "x" TInt)
      "y" TInt
  in
  match tcheck env expr with
  | Ok (TArrow (TInt, TInt)) ->
      Alcotest.(check bool) "typecheck_if_other_than_bool_and_int" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_let1 () =
  let expr =
    Fun ("x", Let ("y", Plus (Var "x", IntLit 10), Times (Var "y", IntLit 10)))
  in
  let env =
    Environment.ext
      (Environment.ext (Environment.emptyEnv ()) "y" TInt)
      "x" TInt
  in
  match tcheck env expr with
  | Ok (TArrow (TInt, TInt)) -> Alcotest.(check bool) "typecheck_let1" true true
  | _ -> Alcotest.fail "typecheck failed"

let test_typecheck_let2 () =
  let expr =
    Let
      ( "x",
        Fun ("y", Var "y"),
        Let ("z", Fun ("w", Var "w"), App (Var "x", Var "z")) )
  in
  let env =
    Environment.ext
      (Environment.ext
         (Environment.ext
            (Environment.ext (Environment.emptyEnv ()) "w" TInt)
            "z"
            (TArrow (TInt, TInt)))
         "y"
         (TArrow (TInt, TInt)))
      "x"
      (TArrow (TArrow (TInt, TInt), TArrow (TInt, TInt)))
  in
  match tcheck env expr with
  | Ok (TArrow (TInt, TInt)) -> Alcotest.(check bool) "typecheck_let2" true true
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
          Alcotest.test_case "test_typecheck_env1" `Quick test_typecheck_env1;
          Alcotest.test_case "test_typecheck_env2" `Quick test_typecheck_env2;
          Alcotest.test_case "test_typecheck_env3" `Quick test_typecheck_env3;
          Alcotest.test_case "test_typecheck_env4" `Quick test_typecheck_env4;
          Alcotest.test_case "test_typecheck_env5" `Quick test_typecheck_env5;
          Alcotest.test_case "test_typecheck_fn1" `Quick test_typecheck_fn1;
          Alcotest.test_case "test_typecheck_fn2" `Quick test_typecheck_fn2;
          Alcotest.test_case "test_typecheck_fn3" `Quick test_typecheck_fn3;
          Alcotest.test_case "test_typecheck_fn4" `Quick test_typecheck_fn4;
          Alcotest.test_case "test_typecheck_fn5" `Quick test_typecheck_fn5;
          Alcotest.test_case "test_typecheckl_if_other_than_bool_and_int" `Quick
            test_typecheck_if_other_than_bool_and_int;
          Alcotest.test_case "test_typecheck_let1" `Quick test_typecheck_let1;
          Alcotest.test_case "test_typecheck_let2" `Quick test_typecheck_let2;
        ] );
    ]
