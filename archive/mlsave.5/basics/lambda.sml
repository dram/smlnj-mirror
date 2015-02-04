(* lambda.sml *)

signature LAMBDA = sig

structure Access: ACCESS
structure Basics: BASICS

datatype con
  = DATAcon of Basics.datacon
  | INTcon of int
  | REALcon of string
  | STRINGcon of string

datatype lexp
  = VAR of Access.lvar
  | FN of Access.lvar * lexp
  | FIX of Access.lvar list * lexp list * lexp
  | APP of lexp * lexp
  | INT of int
  | REAL of string
  | STRING of string
  | SWITCH of lexp * (con*lexp) list * lexp Basics.Option
  | RECORD of lexp list
  | SELECT of int * lexp
  | RAISE of lexp
  | HANDLE of lexp * lexp

 val CON : Basics.datacon * lexp -> lexp
 val DECON : Basics.datacon * lexp -> lexp

end

structure Lambda : LAMBDA = struct 

  structure Access = Access
  structure Basics = Basics
  open Access Basics

 datatype con
  = DATAcon of datacon
  | INTcon of int
  | REALcon of string
  | STRINGcon of string

  datatype lexp
    = VAR of lvar
    | FN of lvar * lexp
    | FIX of lvar list * lexp list * lexp
    | APP of lexp * lexp
    | INT of int
    | REAL of string
    | STRING of string
    | SWITCH of lexp * (con*lexp) list * lexp Option
    | RECORD of lexp list
    | SELECT of int * lexp
    | RAISE of lexp
    | HANDLE of lexp * lexp

val deref = SELECT(Prim.fetchSlot,VAR 0)
val makeref = SELECT(Prim.makerefSlot,VAR 0)

fun trpath [i] = VAR i
  | trpath (a::r) = SELECT(a, trpath r)

fun CON (DATACON{rep=ref (TAGGED i),...},e) = RECORD[e,INT i]
  | CON (DATACON{rep=ref TRANSPARENT,...},e) = e
  | CON (DATACON{rep=ref TRANSU,...},e) = e
  | CON (DATACON{rep=ref TRANSB,...},e) = e
  | CON (DATACON{rep=ref REF,...},e) = APP(makeref,e)
  | CON (DATACON{rep=ref (VARIABLE(PATH p)),...},e) = RECORD[e, trpath p]
  | CON (DATACON{rep=ref (CONSTANT i),...},e) = INT i

fun DECON (DATACON{rep=ref (TAGGED _),...},e) = SELECT(0,e)
  | DECON (DATACON{rep=ref TRANSPARENT,...},e) = e
  | DECON (DATACON{rep=ref TRANSU,...},e) = e
  | DECON (DATACON{rep=ref TRANSB,...},e) = e
  | DECON (DATACON{rep=ref REF,...},e) = APP(deref,e)
  | DECON (DATACON{rep=ref (VARIABLE _),...},e) = SELECT(0,e) 
  | DECON (DATACON{name,rep=ref r,...},e) =
     (print (Symbol.Name name); print " "; PrintBasics.printRep r; print "\n";
     ErrorMsg.Impossible "DECON with bad constructor")

end
