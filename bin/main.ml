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

let () = run "1 + 2 * 3" |> value_to_string |> print_endline
