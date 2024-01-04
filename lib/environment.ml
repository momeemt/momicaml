module Environment = struct
  let emptyEnv () = []

  let ext env x v =
    (x, v) :: env

  let rec lookup x env =
    match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y, v) :: tl -> if x = y then v else lookup x tl
end
