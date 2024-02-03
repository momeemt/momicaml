open Syntax

module Environment : sig
  val emptyEnv : unit -> (string, value) Hashtbl.t

  val ext : (string, value) Hashtbl.t -> string -> value -> (string, value) Hashtbl.t

  val lookup : string -> (string, value) Hashtbl.t -> value
end
