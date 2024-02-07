type ty = TInt | TBool | TArrow of ty * ty | TVar of string
type tyenv = (string, ty) Hashtbl.t

let new_typevar s = TVar ("'" ^ s)

let rec ty_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) -> ty_to_string t1 ^ " -> " ^ ty_to_string t2
  | TVar s -> "TVar(" ^ s ^ ")"

let te_to_string te =
  let l = Hashtbl.fold (fun k v acc -> (k, v) :: acc) te [] in
  let l = List.map (fun (k, v) -> k ^ " : " ^ ty_to_string v) l in
  String.concat ", " l
