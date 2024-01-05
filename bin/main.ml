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
  run "let x = 1 in let f = fun y -> x + y in let x = 2 in f (x + 3)" |> value_to_string |> print_endline;

