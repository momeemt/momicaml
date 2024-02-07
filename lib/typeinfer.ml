open! Environment
open Syntax
open Types
open Result

module TypeInferer = struct
  let substitute tvar t te =
    Hashtbl.iter
      (fun x t2 ->
        let t3 = if t2 = tvar then t else t2 in
        Hashtbl.replace te x t3)
      te;
    te

  let rec tinf te e =
    match e with
    | IntLit _ -> ok (te, TInt)
    | BoolLit _ -> ok (te, TBool)
    | Var s -> (
        let tvar = Environment.lookup s te in
        match tvar with
        | Ok t -> ok (te, t)
        | Error _ ->
            let tvar = new_typevar s in
            let te1 = Environment.ext te s tvar in
            ok (te1, tvar))
    | Plus (e1, e2) -> (
        match tinf te e1 with
        | Ok (te1, t1) -> (
            match
              match t1 with
              | TInt -> Ok te1
              | TVar _ -> Ok (substitute t1 TInt te1)
              | _ -> Error "type error in Plus"
            with
            | Ok te2 -> (
                match tinf te2 e2 with
                | Ok (te3, t2) -> (
                    match
                      match t2 with
                      | TInt -> Ok te3
                      | TVar _ -> Ok (substitute t2 TInt te3)
                      | _ -> Error "type error in Plus"
                    with
                    | Ok te4 -> Ok (te4, TInt)
                    | Error e -> Error e)
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | If (e1, e2, e3) -> (
        match tinf te e1 with
        | Ok (te1, t1) -> (
            let te2 =
              match t1 with
              | TBool -> ok te1
              | TVar _ -> substitute t1 TBool te1 |> ok
              | _ -> error "type error in IF"
            in
            match te2 with
            | Ok te2 -> (
                match tinf te2 e2 with
                | Ok (te3, t2) -> (
                    match tinf te3 e3 with
                    | Ok (te4, t3) -> (
                        Printf.printf "te: %s\n" (te_to_string te);
                        Printf.printf "te1: %s\n" (te_to_string te1);
                        Printf.printf "te2: %s\n" (te_to_string te2);
                        Printf.printf "te3: %s\n" (te_to_string te3);
                        Printf.printf "te4: %s\n" (te_to_string te4);
                        match (t2, t3) with
                        | TInt, TInt -> ok (te4, TInt)
                        | TBool, TBool -> ok (te4, TBool)
                        | TInt, TVar _ ->
                            let te5 = substitute t3 TInt te4 in
                            Printf.printf "te5: %s\n" (te_to_string te5);
                            ok (te5, TInt)
                        | TVar _, TInt ->
                            let te5 = substitute t2 TInt te4 in
                            Printf.printf "te5: %s\n" (te_to_string te5);
                            ok (te5, TInt)
                        | TBool, TVar _ ->
                            let te5 = substitute t3 TBool te4 in
                            Printf.printf "te5: %s\n" (te_to_string te5);
                            ok (te5, TBool)
                        | TVar _, TBool ->
                            let te5 = substitute t2 TBool te4 in
                            Printf.printf "te5: %s\n" (te_to_string te5);
                            ok (te5, TBool)
                        | TVar _, TVar _ ->
                            let te5 = substitute t2 t3 te4 in
                            Printf.printf "te5: %s\n" (te_to_string te5);
                            ok (te5, t3)
                        | _ -> failwith "type error in If")
                    | Error e -> Error e)
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | _ -> error "Not implemented yet"
end
