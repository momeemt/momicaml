module Environment : sig
  val emptyEnv : unit -> (string, 'a) Hashtbl.t

  val ext : (string, 'a) Hashtbl.t -> string -> 'a -> (string, 'a) Hashtbl.t

  val lookup : string -> (string, 'a) Hashtbl.t -> ('a, string) result
end
