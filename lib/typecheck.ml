open Syntax
open Result
open! Environment

type ty = TInt | TBool
type tyenv = (string, ty) Hashtbl.t

let rec tcheck te e =
  match e with
  | Var s -> Environment.lookup s te
  | IntLit _ -> ok TInt
  | BoolLit _ -> ok TBool
  | Plus (e1, e2) -> (
      match (tcheck te e1, tcheck te e2) with
      | Ok TInt, Ok TInt -> ok TInt
      | _ -> error "type error in Plus")
  | Eq (e1, e2) -> (
      match (tcheck te e1, tcheck te e2) with
      | Ok t1, Ok t2 -> if t1 = t2 then ok TBool else error "type error in Eq"
      | _ -> error "type error in Eq")
  | If (e1, e2, e3) -> (
      match (tcheck te e1, tcheck te e2, tcheck te e3) with
      | Ok TBool, Ok t2, Ok t3 ->
          if t2 = t3 then ok t2 else error "type error in If"
      | _ -> error "type error in If")
  | _ -> error "unknown expression"
