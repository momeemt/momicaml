open! Ast

module Eval : sig
  val eval : Ast.exp -> (string * Ast.value) list -> Ast.value
end
