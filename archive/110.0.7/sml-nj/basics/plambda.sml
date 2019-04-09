(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* plambda.sml *)

structure PLambda : PLAMBDA =
struct

local structure A  = Access
      structure LK = LtyKernel
      structure LV = LambdaVar
      structure PO = PrimOp
      structure S  = Symbol
in 

type tkind = LK.tkind
type tyc = LK.tyc
type lty = LK.lty

type lvar = LV.lvar

(*
 * dataconstr records the name of the constructor, the corresponding conrep, 
 * and the lambda type lty; value carrying data constructors would have 
 * arrow type. 
 *)
type dataconstr = S.symbol * A.conrep * lty 

(*
 * con: used to specify all possible switching statements. Efficient switch 
 * generation can be applied to DATAcon and INTcon. Otherwise, it is just a
 * shorthand for binary branch trees. In the future, we probably should make
 * it more general, including constants of any numerical types. 
 *)
datatype con 
  = DATAcon of dataconstr
  | INTcon of int
  | INT32con of Int32.int
  | WORDcon of word
  | WORD32con of Word32.word
  | REALcon of string
  | STRINGcon of string
  | VLENcon of int 

(*
 * scon: the list of static constants; this list is going to grow, of
 * course. We assume that the type of each constant can be inferred 
 * automatically. Also, PRIMOP could be made as part of the static 
 * constants, if ever we want to.
 *
 * datatype scon 
 *   = INT of int
 *   | INT32 of Int32.int
 *   | WORD of word
 *   | WORD32 of Word32.word
 *   | REAL of string
 *   | STRING of string 
 *)

(*
 * lexp: the universal typed intermediate language. TFN, TAPP is abstraction
 * and application on type constructors. Structure abstractions and functor 
 * abstractions are represented as normal structure and functor definitions 
 * with its component properly PACKed. FN defines normal function, FIX defines
 * a set of recursive functions, LET(v,e1,e2) is a syntactic sugar for exprs 
 * of forms like APP(FN(v,_,e2), e1); the type of v will be that of e1. 
 * APP is the function application. STRECD and STRSEL are structure record
 * selection, VECTOR and VCTSEL are vector record and vector selection.
 * EXNF, EXNC, RAISE, and HANDLE are for exceptions.
 *)
datatype lexp
  = VAR of lvar
  | INT of int
  | INT32 of Int32.int
  | WORD of word
  | WORD32 of Word32.word
  | REAL of string
  | STRING of string 
  | PRIM of PO.primop * lty * tyc list
  | GENOP of dict * PO.primop * lty * tyc list
 
  | FN of lvar * lty * lexp
  | FIX of lvar list * lty list * lexp list * lexp
  | APP of lexp * lexp
  | LET of lvar * lexp * lexp

  | TFN of tkind list * lexp
  | TAPP of lexp * tyc list

  | EXNF of lexp * lty                 
  | EXNC of lexp              
  | RAISE of lexp * lty 
  | HANDLE of lexp * lexp

  | CON of dataconstr * tyc list * lexp
  | DECON of dataconstr * tyc list * lexp
  | SWITCH of lexp * A.consig * (con * lexp) list * lexp option

  | VECTOR of lexp list * tyc
  | RECORD of lexp list
  | SRECORD of lexp list    
  | SELECT of int * lexp

  | PACK of lty * tyc list * tyc list * lexp   
  | WRAP of tyc * bool * lexp
  | UNWRAP of tyc * bool * lexp

withtype dict = {default: lexp, table: (tyc list * lexp) list}

end (* local *)
end (* structure Lambda *)

(*
 * $Log: plambda.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/05/06 01:15:32  george
 *   Version 109.27+
 *
 * Revision 1.2  1997/04/18  15:48:13  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:10  george
 *   Version 109.24
 *
 *)
