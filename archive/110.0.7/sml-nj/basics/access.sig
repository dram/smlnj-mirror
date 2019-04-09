(* Copyright 1996 by AT&T Bell Laboratories *)
(* access.sig *)

signature ACCESS = sig

type lvar = LambdaVar.lvar

datatype access
  = LVAR of lvar
  | EXTERN of PersStamps.persstamp
  | PATH of access * int
  | NO_ACCESS

datatype conrep
  = UNTAGGED                             
  | TAGGED of int                        
  | TRANSPARENT                          
  | CONSTANT of int                      
  | REF                                  
  | EXNCONST of access
  | EXNFUN of access
  | LISTCONS                              
  | LISTNIL

datatype consig 
  = CSIG of int * int
  | CNIL

val prAcc   : access -> string
val prRep   : conrep -> string
val prCsig  : consig -> string
val isExn   : conrep -> bool
val isConst : conrep -> bool

val selAcc  : access * int -> access
val dupAcc  : lvar * (Symbol.symbol option -> lvar) -> access

val namedAcc : Symbol.symbol * (Symbol.symbol option -> lvar) 
                 -> access

val newAcc  : (Symbol.symbol option -> lvar) -> access

val extAcc  : PersStamps.persstamp -> access
val nullAcc : access

val accLvar : access -> lvar option

end (* signature ACCESS *)



(*
 * $Log: access.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:09  george
 *   Version 109.24
 *
 *)
