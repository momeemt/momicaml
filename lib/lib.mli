module Environment : sig
  val emptyEnv : unit -> (string * 'a) list
  val ext : (string * 'a) list -> string -> 'a -> (string * 'a) list
  val lookup : string -> (string * 'a) list -> 'a
end

