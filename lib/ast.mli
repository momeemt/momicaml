module Ast : sig
  type exp
  type value
  val exp_abs : exp -> exp
  val string_of_value : value -> string
  val string_of_exp : exp -> string
  val eval : exp -> value
  val binop : (exp -> value) -> (int -> int -> int) -> exp -> exp -> value
end

