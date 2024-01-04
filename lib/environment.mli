open Syntax

module Environment : sig
  val emptyEnv : unit -> (string * value) list

  val ext :
    (string * value) list ->
    string ->
    value ->
    (string * value) list

  val lookup : string -> (string * value) list -> value
end
