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
    | TList t -> TList (subst_ty theta t)

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
            | TList t1, TList t2 -> solve ((t1, t2) :: eql2) theta
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

  let theta0 = Hashtbl.create 10
  let new_typevar n = (TVar ("'a" ^ string_of_int n), n + 1)

  let rec tinf te e n =
    match e with
    | Var s -> (
        match Environment.lookup s te with
        | Ok t -> ok (te, t, theta0, n)
        | Error _ ->
            let tx, n1 = new_typevar n in
            let te1 = Environment.ext te s tx in
            ok (te1, tx, theta0, n1))
    | IntLit _ -> ok (te, TInt, theta0, n)
    | BoolLit _ -> ok (te, TBool, theta0, n)
    | Plus (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, TInt); (t2, TInt) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TInt, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Minus (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, TInt); (t2, TInt) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TInt, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Times (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, TInt); (t2, TInt) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TInt, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Div (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, TInt); (t2, TInt) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TInt, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | If (e1, e2, e3) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                match tinf te2 e3 n2 with
                | Ok (te3, t3, theta3, n3) -> (
                    let t11 = subst_ty theta3 t1 in
                    match unify [ (t11, TBool); (t2, t3) ] with
                    | Ok theta4 ->
                        let te4 = subst_tyenv theta4 te3 in
                        let theta5 =
                          compose_subst theta4
                            (compose_subst theta3 (compose_subst theta2 theta1))
                        in
                        ok (te4, t2, theta5, n3)
                    | Error e -> error e)
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Fun (x, e) -> (
        let tx, n1 = new_typevar n in
        let te1 = Environment.ext te x tx in
        match tinf te1 e n1 with
        | Ok (te2, t1, theta1, n2) ->
            let t2 = subst_ty theta1 tx in
            Hashtbl.remove te2 x;
            ok (te2, TArrow (t2, t1), theta1, n2)
        | Error e -> Error e)
    | App (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let tx, n3 = new_typevar n2 in
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, TArrow (t2, tx)) ] with
                | Ok theta3 ->
                    let t3 = subst_ty theta3 tx in
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, t3, theta4, n3)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Let (x, e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            let te2 = Environment.ext te1 x t1 in
            match tinf te2 e2 n1 with
            | Ok (te3, t2, theta2, n3) ->
                let theta3 = compose_subst theta2 theta1 in
                ok (te3, t2, theta3, n3)
            | Error e -> error e)
        | Error e -> error e)
    | LetRec (f, x, e1, e2) -> (
        let arg_type, n1 = new_typevar n in
        let return_type, n2 = new_typevar n1 in
        let fun_type = TArrow (arg_type, return_type) in
        let te1 = Environment.ext te f fun_type in
        let te2 = Environment.ext te1 x arg_type in
        match tinf te2 e1 n2 with
        | Ok (te3, t1, _, n3) -> (
            match unify [ (return_type, t1) ] with
            | Ok theta2 -> (
                let te4 = subst_tyenv theta2 te3 in
                match tinf te4 e2 n3 with
                | Ok (te5, t2, theta3, n4) ->
                    let theta4 = compose_subst theta3 theta2 in
                    ok (te5, t2, theta4, n4)
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Empty ->
        let tvar, n1 = new_typevar n in
        ok (te, TList tvar, theta0, n1)
    | Cons (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t2, TList t11) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TList t11, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Head e -> (
        match tinf te e n with
        | Ok (te1, t1, theta1, n1) -> (
            match t1 with
            | TList t2 -> ok (te1, t2, theta1, n1)
            | TVar s ->
                let t2, n2 = new_typevar n1 in
                let theta2 = Hashtbl.create 1 in
                Hashtbl.add theta2 s (TList t2);
                let theta3 = compose_subst theta2 theta1 in
                ok (subst_tyenv theta2 te1, t2, theta3, n2)
            | _ -> error "type error in Head")
        | Error e -> Error e)
    | Tail e -> (
        match tinf te e n with
        | Ok (te1, t1, theta1, n1) -> (
            match t1 with
            | TList t2 -> ok (te1, TList t2, theta1, n1)
            | TVar s ->
                let t2, n2 = new_typevar n1 in
                let theta2 = Hashtbl.create 1 in
                Hashtbl.add theta2 s (TList t2);
                let theta3 = compose_subst theta2 theta1 in
                ok (subst_tyenv theta2 te1, TList t2, theta3, n2)
            | _ -> error "type error in Tail")
        | Error e -> Error e)
    | Eq (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, t2) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TBool, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Neq (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, t2) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TBool, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Greater (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, t2) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TBool, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Less (e1, e2) -> (
        match tinf te e1 n with
        | Ok (te1, t1, theta1, n1) -> (
            match tinf te1 e2 n1 with
            | Ok (te2, t2, theta2, n2) -> (
                let t11 = subst_ty theta2 t1 in
                match unify [ (t11, t2) ] with
                | Ok theta3 ->
                    let te3 = subst_tyenv theta3 te2 in
                    let theta4 =
                      compose_subst theta3 (compose_subst theta2 theta1)
                    in
                    ok (te3, TBool, theta4, n2)
                | Error e -> error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | Match (e, (elist : (exp * exp) list)) -> (
        match tinf te e n with
        | Ok (te1, t1, theta1, n1) ->
            let t2, n2 = new_typevar n1 in
            let rec tinf_match te elist theta n =
              match elist with
              | [] -> ok (te, t2, theta, n)
              | (e1, e2) :: elist2 -> (
                  match tinf te e1 n with
                  | Ok (te1, t3, theta2, n2) -> (
                      match tinf te1 e2 n2 with
                      | Ok (te2, t4, theta3, n3) -> (
                          let theta4 = compose_subst theta3 theta2 in
                          let theta5 = compose_subst theta4 theta in
                          match unify [ (t3, t1); (t4, t2) ] with
                          | Ok theta6 ->
                              let theta_final = compose_subst theta6 theta5 in
                              let te_final = subst_tyenv theta_final te2 in
                              tinf_match te_final elist2 theta_final n3
                          | Error e -> error e)
                      | Error e -> Error e)
                  | Error e -> Error e)
            in
            tinf_match te1 elist theta1 n2
        | Error e -> Error e)

  let tinf_top e = tinf (Environment.emptyEnv ()) e 0

  let rec tinf_old te e =
    match e with
    | IntLit _ -> ok (te, TInt)
    | BoolLit _ -> ok (te, TBool)
    | Var s -> (
        let tvar = Environment.lookup s te in
        match tvar with
        | Ok t -> ok (te, t)
        | Error _ ->
            let tvar = Types.new_typevar s in
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
                        match (t2, t3) with
                        | TInt, TInt -> ok (te4, TInt)
                        | TBool, TBool -> ok (te4, TBool)
                        | TInt, TVar _ ->
                            let te5 = substitute t3 TInt te4 in
                            ok (te5, TInt)
                        | TVar _, TInt ->
                            let te5 = substitute t2 TInt te4 in
                            ok (te5, TInt)
                        | TBool, TVar _ ->
                            let te5 = substitute t3 TBool te4 in
                            ok (te5, TBool)
                        | TVar _, TBool ->
                            let te5 = substitute t2 TBool te4 in
                            ok (te5, TBool)
                        | TVar _, TVar _ ->
                            let te5 = substitute t2 t3 te4 in
                            ok (te5, t3)
                        | _ -> failwith "type error in If")
                    | Error e -> Error e)
                | Error e -> Error e)
            | Error e -> Error e)
        | Error e -> Error e)
    | _ -> error "Not implemented yet"
end
