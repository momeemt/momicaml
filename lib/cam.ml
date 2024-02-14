open Result

module CAM = struct
  type cam_instr =
    | CAM_Ldi of int
    | CAM_Ldb of bool
    | CAM_Access of int
    | CAM_Closure of cam_code
    | CAM_Apply
    | CAM_Return
    | CAM_Let
    | CAM_EndLet
    | CAM_Test of cam_code * cam_code
    | CAM_Add
    | CAM_Sub
    | CAM_Mult
    | CAM_Div
    | CAM_Eq

  and cam_code = cam_instr list

  type cam_value =
    | CAM_IntCal of int
    | CAM_BoolVal of bool
    | CAM_ClosVal of cam_code * cam_env

  and cam_stack = cam_value list
  and cam_env = cam_value list

  let rec string_of_code code =
    String.concat " "
      (List.map
         (function
           | CAM_Ldi n -> "Ldi(" ^ string_of_int n ^ ")"
           | CAM_Ldb b -> "Ldb(" ^ string_of_bool b ^ ")"
           | CAM_Access i -> "Access(" ^ string_of_int i ^ ")"
           | CAM_Closure c -> "Closure (" ^ string_of_code c ^ ")"
           | CAM_Apply -> "Apply"
           | CAM_Return -> "Return"
           | CAM_Let -> "Let ("
           | CAM_EndLet -> ") EndLet"
           | CAM_Test (a, b) ->
               "Test (" ^ string_of_code a ^ ", " ^ string_of_code b ^ ")"
           | CAM_Add -> "Add"
           | CAM_Sub -> "Sub"
           | CAM_Mult -> "Mult"
           | CAM_Div -> "Div"
           | CAM_Eq -> "Eq")
         code)

  let string_of_value = function
    | CAM_IntCal n -> string_of_int n
    | CAM_BoolVal b -> string_of_bool b
    | CAM_ClosVal _ -> "closure"

  let rec inner_eval code env stack =
    match code with
    | Ok [] -> (ok [], env, stack)
    | Ok (inst :: c) -> (
        match inst with
        | CAM_Ldi n -> (ok c, env, CAM_IntCal n :: stack)
        | CAM_Ldb b -> (ok c, env, CAM_BoolVal b :: stack)
        | CAM_Access i -> (ok c, env, List.nth env i :: stack)
        | CAM_Closure c' -> (ok c, env, CAM_ClosVal (c', env) :: stack)
        | CAM_Apply -> (
            match stack with
            | CAM_ClosVal (c', env') :: v :: s ->
                ( ok c',
                  v :: CAM_ClosVal (c', env') :: env,
                  CAM_ClosVal (c, env) :: s )
            | _ -> (error "Type error in CAM_Apply", [], []))
        | CAM_Return -> (
            match stack with
            | v :: CAM_ClosVal (c', env') :: s -> (ok c', env', v :: s)
            | _ -> (error "Type error in CAM_Return", [], []))
        | CAM_Let -> (
            match stack with
            | v :: s -> (ok c, v :: env, s)
            | _ -> (error "Type error in CAM_Let", [], []))
        | CAM_EndLet -> (
            match env with
            | _ :: env' -> (ok c, env', stack)
            | _ -> (error "Type error in CAM_EndLet", [], []))
        | CAM_Test (c1, c2) -> (
            match stack with
            | CAM_BoolVal true :: s -> (ok (c1 @ c), env, s)
            | CAM_BoolVal false :: s -> (ok (c2 @ c), env, s)
            | _ -> (error "Type error in CAM_Test", [], []))
        | CAM_Add -> (
            match stack with
            | CAM_IntCal n1 :: CAM_IntCal n2 :: s ->
                (ok c, env, CAM_IntCal (n1 + n2) :: s)
            | _ -> (error "Type error in CAM_Add", [], []))
        | CAM_Sub -> (
            match stack with
            | CAM_IntCal n1 :: CAM_IntCal n2 :: s ->
                (ok c, env, CAM_IntCal (n1 - n2) :: s)
            | _ -> (error "Type error in CAM_Sub", [], []))
        | CAM_Mult -> (
            match stack with
            | CAM_IntCal n1 :: CAM_IntCal n2 :: s ->
                (ok c, env, CAM_IntCal (n1 * n2) :: s)
            | _ -> (error "Type error in CAM_Mult", [], []))
        | CAM_Div -> (
            match stack with
            | CAM_IntCal _ :: CAM_IntCal 0 :: _ ->
                (error "Division by zero", [], [])
            | CAM_IntCal n1 :: CAM_IntCal n2 :: s ->
                (ok c, env, CAM_IntCal (n1 / n2) :: s)
            | _ -> (error "Type error in CAM_Div", [], []))
        | CAM_Eq -> (
            match stack with
            | CAM_IntCal n1 :: CAM_IntCal n2 :: s ->
                (ok c, env, CAM_BoolVal (n1 = n2) :: s)
            | _ -> (error "Type error in CAM_Eq", [], [])))
    | Error e -> (error e, [], [])

  and eval code env stack =
    match code with
    | Ok _ -> (
        let new_code, new_env, new_stack = inner_eval code env stack in
        match new_code with
        | Ok [] -> ok new_stack
        | Ok _ -> eval new_code new_env new_stack
        | Error e -> error e)
    | Error e -> error e
end
