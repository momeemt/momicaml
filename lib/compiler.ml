open Cam
open Typeinfer
open Typecheck
open! Codegen
open Result

module Compiler = struct
  let parse str = Parser.main Lexer.token (Lexing.from_string str)

  let compile str =
    let ast = parse str in
    match TypeInferer.tinf_top ast with
    | Ok (te, _, _, _) -> (
        let ast_tchecked = tcheck te ast in
        match ast_tchecked with
        | Ok _ -> (
            let cam = Codegen.cam_codegen (ok ast) [] in
            match CAM.eval cam [] [] with
            | Ok stack -> (
                match List.hd stack with
                | CAM.CAM_IntCal i -> string_of_int i |> ok
                | CAM.CAM_BoolVal b -> string_of_bool b |> ok
                | CAM.CAM_ClosVal _ -> ok "closure")
            | Error e -> error e)
        | Error e -> error e)
    | Error e -> error e
end
