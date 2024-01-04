open Syntax

module Eval : sig
  val eval : exp -> (string * value) list -> value
end
