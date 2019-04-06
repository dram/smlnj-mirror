(* access.sig *)

signature ACCESS = sig

  structure Symbol : SYMBOL

  type lvar  (* lambda variable id *)
  type slot  (* position in structure record *)
  type path  (* slot chain relative to lambda variable *)
  
  datatype access 
    = LVAR of lvar  
    | SLOT of slot
    | PATH of path
    | INLINE of int
  
  val mkLvar : unit -> lvar
  val sameName : lvar * lvar -> unit
  val dupLvar : lvar -> lvar
  val namedLvar : Symbol.symbol -> lvar
  val lvarName : lvar -> string
  val rootLvarName : lvar -> string
  val resetLvars : lvar -> unit
  val saveLvarNames : bool ref

end (* signature ACCESS *)
