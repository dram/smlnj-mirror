(* Copyright 1989 by AT&T Bell Laboratories *)
(* env.sig *)

signature ENV =
sig
  structure Symbol : SYMBOL
  type 'b env
  exception Unbound  
  exception SpecialEnv
  val empty: 'b env
  val look: 'b env -> Symbol.symbol -> 'b
  val bind: Symbol.symbol * 'b * 'b env -> 'b env
  val open': 'b env * ('b->'b) * 'b env -> 'b env
  val special: (Symbol.symbol -> 'b) * 'b env -> 'b env
  infixr atop
  val atop: 'b env * 'b env -> 'b env
  val consolidate: '1b env -> '1b env
  val app: (Symbol.symbol * 'b -> unit) -> 'b env -> unit
  val map: ('1b -> '1b) -> '1b env -> '1b env
end
