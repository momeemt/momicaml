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
    | CAM_Eq
  and cam_code = cam_instr list

  type cam_value =
    | CAM_IntCal of int
    | CAM_BoolVal of bool
    | CAM_ClosVal of cam_code * cam_env
  and cam_stack = cam_value list
  and cam_env = cam_value list

  let string_of_code code =
    String.concat " " (List.map (function
      | CAM_Ldi n -> "Ldi " ^ string_of_int n
      | CAM_Ldb b -> "Ldb " ^ string_of_bool b
      | CAM_Access i -> "Access " ^ string_of_int i
      | CAM_Closure _ -> "Closure"
      | CAM_Apply -> "Apply"
      | CAM_Return -> "Return"
      | CAM_Let -> "Let"
      | CAM_EndLet -> "EndLet"
      | CAM_Test _ -> "Test"
      | CAM_Add -> "Add"
      | CAM_Eq -> "Eq"
    ) code)

  let string_of_value = function
    | CAM_IntCal n -> string_of_int n
    | CAM_BoolVal b -> string_of_bool b
    | CAM_ClosVal _ -> "closure"

  let rec inner_eval code env stack =
    match code with
    | [] -> code, env, stack
    | inst::c -> 
      match inst with
      | CAM_Ldi n -> c, env, (CAM_IntCal n::stack)
      | CAM_Ldb b -> c, env, (CAM_BoolVal b::stack)
      | CAM_Access i -> c, env, (List.nth env i::stack)
      | CAM_Closure c' -> c, env, (CAM_ClosVal (c', env)::stack)
      | CAM_Apply -> (
        match stack with
        | CAM_ClosVal (c', env')::v::s -> c', v :: CAM_ClosVal(c', env') :: env, CAM_ClosVal(c, env) :: s
        | _ -> failwith "Type error"
      )
      | CAM_Return -> (
        match stack with
        | v::CAM_ClosVal (c', env')::s -> c', env', v::s
        | _ -> failwith "Type error"
      )
      | CAM_Let -> (
        match stack with
        | v::s -> c, v::env, s
        | _ -> failwith "Type error"
      )
      | CAM_EndLet -> (
        match env with
        | _::env' -> c, env', stack
        | _ -> failwith "Type error"
      )
      | CAM_Test (c1, c2) -> (
        match stack with
        | CAM_BoolVal true::s -> c1 @ c, env, s
        | CAM_BoolVal false::s -> c2 @ c, env, s
        | _ -> failwith "Type error"
      )
      | CAM_Add -> (
        match stack with
        | CAM_IntCal n1::CAM_IntCal n2::s -> c, env, CAM_IntCal (n1 + n2)::s
        | _ -> failwith "Type error"
      )
      | CAM_Eq -> (
        match stack with
        | CAM_IntCal n1::CAM_IntCal n2::s -> c, env, CAM_BoolVal (n1 = n2)::s
        | _ -> failwith "Type error"
      )
  and eval code env stack =
    let
      (new_code, new_env, new_stack) = inner_eval code env stack
    in
    match new_code with
    | [] -> new_stack
    | _ -> eval new_code new_env new_stack
end
