open Syntax
open Cam
open Result
open Option
open List

module Codegen = struct
  let rec position x venv =
    match venv with
    | [] -> failwith "no matching variable in environment"
    | (y, _) :: venv2 -> if x = y then 0 else 1 + position x venv2

  let rec cam_codegen exp venv =
    match exp with
    | Ok (Var x) -> (
        let pos = position x venv in
        let (_, value) = List.nth venv pos in
        match value with
        | Some (CAM.CAM_Ldi i) -> ok [ CAM.CAM_Ldi i ]
        | Some (CAM.CAM_Ldb b) -> ok [ CAM.CAM_Ldb b ]
        | Some _ | None -> ok [ CAM.CAM_Access pos ]
    )
    | Ok (Fun (x, e)) -> (
        let res = cam_codegen (ok e) ((x, none) :: ("_", none) :: venv) in
        match res with
        | Ok code -> ok [ CAM.CAM_Closure (code @ [ CAM.CAM_Return ]) ]
        | Error e -> Error e)
    | Ok (App (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Apply ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (Let (x, e1, e2)) -> (
        let res1 = cam_codegen (ok e1) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = if List.length code1 = 1 then
                cam_codegen (ok e2) ((x, List.nth code1 0 |> some) :: venv)
            else
                cam_codegen (ok e2) ((x, none) :: venv)
            in
            match res2 with
            | Ok code2 ->
                ok (code1 @ [ CAM.CAM_Let ] @ code2 @ [ CAM.CAM_EndLet ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (LetRec (f, x, e1, e2)) -> (
        let res1 = cam_codegen (ok e1) ((x, none) :: (f, none) :: venv) in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e2) ((f, none) :: venv) in
            match res2 with
            | Ok code2 ->
                ok
                  ([ CAM.CAM_Closure (code1 @ [ CAM.CAM_Return ]) ]
                  @ [ CAM.CAM_Let ] @ code2 @ [ CAM.CAM_EndLet ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (IntLit n) -> ok [ CAM.CAM_Ldi n ]
    | Ok (BoolLit b) -> ok [ CAM.CAM_Ldb b ]
    | Ok (Plus (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        let res2 = cam_codegen (ok e1) venv in
        match (res1, res2) with
        | (Ok [CAM.CAM_Ldi i1], Ok [CAM.CAM_Ldi i2]) -> ok [CAM.CAM_Ldi (i1 + i2)]
        | (Ok code1, Ok code2) -> ok (code1 @ code2 @ [CAM.CAM_Add])
        | (Error e, _) | (_, Error e) -> error e
    )
    | Ok (Minus (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        let res2 = cam_codegen (ok e1) venv in
        match (res1, res2) with
        | (Ok [CAM.CAM_Ldi i1], Ok [CAM.CAM_Ldi i2]) -> ok [CAM.CAM_Ldi (i1 - i2)]
        | (Ok code1, Ok code2) -> ok (code1 @ code2 @ [CAM.CAM_Sub])
        | (Error e, _) | (_, Error e) -> error e
    )
    | Ok (Times (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        let res2 = cam_codegen (ok e1) venv in
        match (res1, res2) with
        | (Ok [CAM.CAM_Ldi i1], Ok [CAM.CAM_Ldi i2]) -> ok [CAM.CAM_Ldi (i1 * i2)]
        | (Ok code1, Ok code2) -> ok (code1 @ code2 @ [CAM.CAM_Mult])
        | (Error e, _) | (_, Error e) -> error e
    )
    | Ok (Div (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        let res2 = cam_codegen (ok e1) venv in
        match (res1, res2) with
        | (Ok [CAM.CAM_Ldi i1], Ok [CAM.CAM_Ldi i2]) when i1 != 0 -> ok [CAM.CAM_Ldi (i1 / i2)]
        | (Ok code1, Ok code2) -> ok (code1 @ code2 @ [CAM.CAM_Div])
        | (Error e, _) | (_, Error e) -> error e
    )
    | Ok (Eq (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Eq ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (If (e1, e2, e3)) -> (
        let res1 = cam_codegen (ok e1) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e2) venv in
            match res2 with
            | Ok code2 -> (
                let res3 = cam_codegen (ok e3) venv in
                match res3 with
                | Ok code3 -> ok (code1 @ [ CAM.CAM_Test (code2, code3) ])
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok _ -> error "Not implemented"
    | Error e -> Error e
end
