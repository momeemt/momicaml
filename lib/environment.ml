open! Ast

module Environment = struct
  let emptyEnv () = []

  let ext env x v =
    let rec print_env env =
      match env with
      | [] -> ()
      | (x, v) :: tl -> 
        print_string (x ^ " = "); 
        print_string (Ast.string_of_value v); 
        print_string "\n"; 
        print_env tl
    in
    let _ = print_env ((x, v) :: env) in
    (x, v) :: env

  let rec lookup x env =
     match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y, v) :: tl -> if x = y then v else lookup x tl
end
