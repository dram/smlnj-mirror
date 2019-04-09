(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* lambda.sml *)

structure Lambda : LAMBDA =
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
 * dataconstr records the name of the constructor (for debugging), the 
 * corresponding conrep, and the lambda type lty; value carrying data 
 * constructors must have the arrow type. 
 *)
type dataconstr = S.symbol * A.conrep * lty 

(*
 * con: used to specify all possible switching statements. Efficient switch 
 * generation can be applied to DATAcon and INTcon. Otherwise, it is just a
 * shorthand for binary branch trees. REALcon probably should go away very
 * soon. 
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
 * A temporary hack used for built-in polymorphic equality and 
 * array creations.
 *)
type dict = {default: lvar, table: (tyc list * lvar) list}

(* 
 * value: the simple values, including the variables, the primops, and 
 * all the static constants. We assume that the lambda types can be
 * inferred automatically. GENOP(dict,p,t,tc) are the special polymorphic 
 * primops such as equality and array creations, instantiated with the 
 * constructor tc; we expect this hack will go away in the future version 
 * of the compiler.
 *)
datatype value
  = VAR of lvar
  | INT of int
  | INT32 of Int32.int
  | WORD of word
  | WORD32 of Word32.word
  | REAL of string
  | STRING of string 
  | PRIM of PO.primop * lty * tyc list
  | GENOP of dict * PO.primop * lty * tyc list 

(*
 * lexp: the comon typed intermediate language. TFN and TAPP are abstraction
 * and application on type constructors. Structure abstractions and functor 
 * abstractions are represented as normal structure and functor definitions 
 * with its component properly PACKed. FN defines normal function, FIX defines
 * a set of recursive functions, LET(v,e1,e2) is a syntactic sugar for exprs 
 * of forms like APP(FN(v,_,e2), e1); the type of v will be that of e1. 
 * APP is the function application. SRECORD is the structure record, VECTOR
 * is the vector record. EXNF, EXNC, RAISE, and HANDLE are for exceptions.
 *)
datatype lexp
  = SVAL of value
  | FN of lvar * lty * lexp
  | APP of value * value
  | FIX of lvar list * lty list * lexp list * lexp
  | LET of lvar * lexp * lexp

  | TFN of tkind list * lexp
  | TAPP of value * tyc list

  | VECTOR of value list * tyc
  | RECORD of value list
  | SRECORD of value list    
  | SELECT of int * value

  | CON of dataconstr * tyc list * value
  | DECON of dataconstr * tyc list * value
  | SWITCH of value * A.consig * (con * lexp) list * lexp option

  | EXNF of value * lty                 
  | EXNC of value
  | RAISE of value * lty 
  | HANDLE of lexp * value

  | PACK of lty * tyc list * tyc list * value
  | WRAP of tyc * bool * value
  | UNWRAP of tyc * bool * value

end (* local *)
end (* structure Lambda *)

