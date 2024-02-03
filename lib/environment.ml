module Environment = struct
  let emptyEnv () = Hashtbl.create 10

  let ext env x v =
    Hashtbl.add env x v;
    env

  let lookup x env =
    try Hashtbl.find env x
    with Not_found -> failwith("unbound variable: " ^ x)
end
