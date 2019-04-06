(* Copyright 1989 by AT&T Bell Laboratories *)
(* misc.sig *)

signature MISC =
sig

  val ASTERISKsym : Symbol.symbol
  val EQUALsym : Symbol.symbol

  val for : 'a list -> ('a -> 'b) -> unit

  val sort3 : (Symbol.symbol * 'a * 'b) list -> (Symbol.symbol * 'a * 'b) list

  val protect : ((unit -> 'a) * ('a -> unit)) * (unit -> 'b) -> 'b

  val protectScope : (unit -> Env.env) * (Env.env -> unit)

  val bogusID : Symbol.symbol
  val bogusExnID : Symbol.symbol
  val bogusExp: BareAbsn.exp

  val anonName : Symbol.symbol
  val anonParamName : Symbol.symbol
  val nullSigStamp : int
  val nullSig : Basics.Structure

  val nullStr : Basics.Structure
  val nullParamVar : Basics.structureVar 

  val discard : 'a -> unit
  val single : 'a -> 'a list

  val varcon : Basics.binding -> BareAbsn.exp

  val getSTRpath: Symbol.symbol list * ErrorMsg.complainer -> Basics.structureVar
  val checkbound: TyvarSet.tyvarset * Basics.tyvar list * ErrorMsg.complainer -> unit
end
