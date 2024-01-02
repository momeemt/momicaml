open! Ast

module Environment : sig
  val emptyEnv : unit -> (string * 'a) list

  val ext :
    (string * Ast.value) list ->
    string ->
    Ast.value ->
    (string * Ast.value) list

  val lookup : string -> (string * 'a) list -> 'a
end
