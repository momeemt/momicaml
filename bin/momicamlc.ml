open Momicaml.Compiler
open Result

let () =
  let filename = Sys.argv.(1) in
  let file = open_in filename in
  let content = input_line file in
  let result = Compiler.compile content in
  match result with
  | Ok res -> print_endline res
  | Error err -> print_endline err; exit 1
