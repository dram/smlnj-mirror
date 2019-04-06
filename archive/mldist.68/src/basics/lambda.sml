(* Copyright 1989 by AT&T Bell Laboratories *)
(* lambda.sml *)

signature LAMBDA = sig

type primop sharing type primop = Access.primop

datatype con
  = DATAcon of (Symbol.symbol * Access.conrep)
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
  | SWITCH of lexp * Access.conrep list *
               (con * lexp) list * lexp option
  | CON of (Symbol.symbol * Access.conrep) * lexp
  | DECON of (Symbol.symbol * Access.conrep) * lexp
  | RECORD of lexp list
  | SELECT of int * lexp
  | RAISE of lexp
  | HANDLE of lexp * lexp
  | PRIM of primop

  val CON' : (Symbol.symbol * Access.conrep) * lexp -> lexp
  val DECON' : (Symbol.symbol * Access.conrep) * lexp -> lexp

end

structure Lambda : LAMBDA = struct 

  open Access Basics

 type primop = Access.primop

 type datacon = Symbol.symbol * Access.conrep

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
    | SWITCH of lexp * conrep list * (con*lexp) list * lexp option
    | CON of datacon * lexp
    | DECON of datacon * lexp
    | RECORD of lexp list
    | SELECT of int * lexp
    | RAISE of lexp
    | HANDLE of lexp * lexp
    | PRIM of primop

fun trpath [i] = VAR i
  | trpath (a::r) = SELECT(a, trpath r)

 fun CON' ((_,REF),e) = APP(PRIM Access.P.makeref,e)
   | CON' x = CON x

 fun DECON' ((_,REF),e) = APP(PRIM Access.P.!,e)
   | DECON' x = DECON x

end
