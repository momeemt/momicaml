open Momicaml.Eval
open Momicaml.Environment
open Momicaml.Syntax

let parse str =
  Momicaml.Parser.main Momicaml.Lexer.token (Lexing.from_string str)

let run str =
  let env = Environment.emptyEnv () in
  Eval.eval (parse str) env

let value_to_string = function
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | _ -> "?"

let () =
  run "let rec f x = x in 0" |> value_to_string |> print_endline;
  run "let rec f x = x in f 0" |> value_to_string |> print_endline;
  run "let rec f x = if x = 0 then 1 else 2 + f (x - 1) in f 0" |> value_to_string |> print_endline;
  run "let rec f x = if x = 0 then 1 else x * f (x - 1) in f 3" |> value_to_string |> print_endline;
  run "let rec f x = if x = 0 then 1 else x * f (x - 1) in f 5" |> value_to_string |> print_endline;
  run "let rec fib x = if x = 0 then 0 else if x = 1 then 1 else fib (x - 1) + fib (x - 2) in fib 10" |> value_to_string |> print_endline;

