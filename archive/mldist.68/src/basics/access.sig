(* Copyright 1989 by AT&T Bell Laboratories *)
(* access.sig *)

signature ACCESS = sig

  structure P : PRIMOP
  type primop
  eqtype lvar  (* lambda variable id *)
  type slot  (* position in structure record *)
  type path  (* slot chain relative to lambda variable *)

  datatype access 
    = SLOT of slot
    | PATH of path  
    | INLINE of primop
  
  datatype conrep
      = UNDECIDED 
      | TAGGED of int 
      | CONSTANT of int 
      | TRANSPARENT 
      | TRANSU
      | TRANSB
      | REF
      | VARIABLE of access (* exception constructor *)
      | VARIABLEc of access (* exception constructor without argument *)

  val mkLvar : unit -> lvar
  val sameName : lvar * lvar -> unit
  val dupLvar : lvar -> lvar
  val namedLvar : Symbol.symbol -> lvar
  val lvarName : lvar -> string
  val saveLvarNames : bool ref

  val pr_lvar: lvar-> string
  and pr_slot: slot -> string
  and pr_path: path-> string
  and pr_access: access-> string

end (* signature ACCESS *)
