open Syntax

module Eval : sig
  val eval : exp -> (string, value) Hashtbl.t -> value
end
