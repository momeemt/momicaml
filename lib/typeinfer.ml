open! Environment
open Syntax
open Types
open Result

module TypeInferer = struct
  let rec occurs tx t =
    match t with
    | TVar s -> tx = s
    | TArrow (t1, t2) -> occurs tx t1 || occurs tx t2
    | _ -> false

  let rec subst_ty theta t =
    match t with
    | (TInt | TBool) as t -> t
    | TArrow (t1, t2) -> TArrow (subst_ty theta t1, subst_ty theta t2)
    | TVar s -> ( try Hashtbl.find theta s with Not_found -> TVar s)

  let subst_tyenv theta te =
    Hashtbl.fold
      (fun x t acc ->
        Hashtbl.add acc x (subst_ty theta t);
        acc)
      te
      (Hashtbl.create (Hashtbl.length te))

  let subst_eql theta eql =
    List.map (fun (t1, t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

  let compose_subst theta2 theta1 =
    let theta = Hashtbl.copy theta1 in
    Hashtbl.iter
      (fun tx t -> Hashtbl.replace theta tx (subst_ty theta2 t))
      theta1;
    Hashtbl.iter
      (fun tx t -> if not (Hashtbl.mem theta tx) then Hashtbl.add theta tx t)
      theta2;
    theta

  let unify eql =
    let rec solve eql theta =
      match eql with
      | [] -> ok theta
      | (t1, t2) :: eql2 -> (
          if t1 = t2 then solve eql2 theta
          else
            match (t1, t2) with
            | TArrow (t11, t12), TArrow (t21, t22) ->
                solve ((t11, t21) :: (t12, t22) :: eql2) theta
            | TVar s, _ ->
                if occurs s t2 then error "unification failed"
                else
                  let theta_prime = Hashtbl.create 1 in
                  Hashtbl.add theta_prime s t2;
                  let theta_updated = compose_subst theta_prime theta in
                  solve (subst_eql theta_prime eql2) theta_updated
            | _, TVar s ->
                if occurs s t1 then error "unification failed"
                else
                  let theta_prime = Hashtbl.create 1 in
                  Hashtbl.add theta_prime s t1;
                  let theta_updated = compose_subst theta_prime theta in
                  solve (subst_eql theta_prime eql2) theta_updated
            | _, _ -> error "unification failed")
    in
    solve eql (Hashtbl.create 10)

  let substitute tvar t te =
    Hashtbl.iter
      (fun x t2 ->
        let t3 = if t2 = tvar then t else t2 in
        Hashtbl.replace te x t3)
      te;
    te

  let rec tinf_old te e =
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
        match tinf_old te e1 with
        | Ok (te1, t1) -> (
            match
              match t1 with
              | TInt -> Ok te1
              | TVar _ -> Ok (substitute t1 TInt te1)
              | _ -> Error "type error in Plus"
            with
            | Ok te2 -> (
                match tinf_old te2 e2 with
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
        match tinf_old te e1 with
        | Ok (te1, t1) -> (
            let te2 =
              match t1 with
              | TBool -> ok te1
              | TVar _ -> substitute t1 TBool te1 |> ok
              | _ -> error "type error in IF"
            in
            match te2 with
            | Ok te2 -> (
                match tinf_old te2 e2 with
                | Ok (te3, t2) -> (
                    match tinf_old te3 e3 with
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
