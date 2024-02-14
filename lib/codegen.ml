open Syntax
open Cam
open Result

module Codegen = struct
  let rec position x venv =
    match venv with
    | [] -> failwith "no matching variable in environment"
    | y :: venv2 -> if x = y then 0 else 1 + position x venv2
  
  let rec cam_codegen exp venv =
    match exp with
    | Ok (Var x) -> ok [ CAM.CAM_Access (position x venv) ]
    | Ok (Fun (x, e)) -> (
        let res = cam_codegen (ok e) (x :: "_" :: venv) in
        match res with
        | Ok code -> ok ([CAM.CAM_Closure (code @ [ CAM.CAM_Return ])])
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
            let res2 = cam_codegen (ok e2) (x :: venv) in
            match res2 with
            | Ok code2 ->
                ok (code1 @ [ CAM.CAM_Let ] @ code2 @ [ CAM.CAM_EndLet ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (LetRec (f, x, e1, e2)) -> (
        let res1 = cam_codegen (ok e1) (x :: f :: venv) in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e2) (f :: venv) in
            match res2 with
            | Ok code2 ->
                ok
                  ([CAM.CAM_Closure (code1 @ [CAM.CAM_Return])]
                  @ [ CAM.CAM_Let ] @ code2 @ [ CAM.CAM_EndLet ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (IntLit n) -> ok [ CAM.CAM_Ldi n ]
    | Ok (BoolLit b) -> ok [ CAM.CAM_Ldb b ]
    | Ok (Plus (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Add ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (Minus (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Sub ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (Times (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Mult ])
            | Error e -> Error e)
        | Error e -> Error e)
    | Ok (Div (e1, e2)) -> (
        let res1 = cam_codegen (ok e2) venv in
        match res1 with
        | Ok code1 -> (
            let res2 = cam_codegen (ok e1) venv in
            match res2 with
            | Ok code2 -> ok (code1 @ code2 @ [ CAM.CAM_Div ])
            | Error e -> Error e)
        | Error e -> Error e)
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
