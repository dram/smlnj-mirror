(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* lambda.sig *)

signature LAMBDA = sig

type tkind = LtyKernel.tkind
type tyc = LtyKernel.tyc
type lty = LtyKernel.lty
type lvar = LambdaVar.lvar

type dataconstr = Symbol.symbol * Access.conrep * lty
type dict = {default: lvar, table: (tyc list * lvar) list}

datatype con 
  = DATAcon of dataconstr
  | INTcon of int
  | INT32con of Int32.int
  | WORDcon of word
  | WORD32con of Word32.word
  | REALcon of string
  | STRINGcon of string
  | VLENcon of int 

datatype value
  = VAR of lvar
  | INT of int
  | INT32 of Int32.int
  | WORD of word
  | WORD32 of Word32.word
  | REAL of string
  | STRING of string 
  | PRIM of PrimOp.primop * lty * tyc list
  | GENOP of dict * PrimOp.primop * lty * tyc list

datatype lexp
  = SVAL of value
  | FN of lvar * lty * lexp
  | APP of value * value
  | FIX of lvar  list * lty list * lexp list * lexp
  | LET of lvar * lexp * lexp

  | TFN of tkind list * lexp
  | TAPP of value * tyc list

  | VECTOR of value list * tyc
  | RECORD of value list
  | SRECORD of value list    
  | SELECT of int * value

  | CON of dataconstr * tyc list * value
  | DECON of dataconstr * tyc list * value
  | SWITCH of value * Access.consig * (con * lexp) list * lexp option

  | EXNF of value * lty                 
  | EXNC of value
  | RAISE of value * lty 
  | HANDLE of lexp * value

  | PACK of lty * tyc list * tyc list * value
  | WRAP of tyc * bool * value
  | UNWRAP of tyc * bool * value

end (* signature LAMBDA *)

