(*
 * This takes a bunch of RTL and build a database that can be reused.
 *)
signature MD_BUILD_RTL =
sig
   structure L : LAMBDA_RTL
   type ty = int

   val map   : int -> ('a -> 'b) -> 'a list -> 'b list 

   val fetch : ty -> L.loc -> L.exp
   val :=    : ty -> L.loc * L.exp -> L.stm
   val aggb  : ty * ty -> L.cell -> L.loc 
   val aggl  : ty * ty -> L.cell -> L.loc
   val idaggr: ty -> L.cell -> L.loc
   val !     : string -> L.exp
   val $     : string * ty -> L.exp -> L.cell 

   val intConst   : ty -> int -> L.exp
   val wordConst  : ty -> Word32.word -> L.exp

   val newOp : string -> L.exp list -> L.exp
   val newCond : string -> L.exp list -> L.exp


   val operand : ty -> string -> L.exp
   val label   : ty -> string -> L.exp
   val ?       : ty -> L.exp

   val not   : L.cond -> L.cond
   val False : L.cond
   val True  : L.cond

   val sx    : ty * ty -> L.exp -> L.exp
   val zx    : ty * ty -> L.exp -> L.exp
   val bitslice : ty -> (int * int) list -> L.exp -> L.exp

   (* Integer operators *)
   val ~     : ty -> L.exp -> L.exp
   val +     : ty -> L.exp * L.exp -> L.exp
   val -     : ty -> L.exp * L.exp -> L.exp
   val muls  : ty -> L.exp * L.exp -> L.exp
   val mulu  : ty -> L.exp * L.exp -> L.exp
   val divs  : ty -> L.exp * L.exp -> L.exp
   val divu  : ty -> L.exp * L.exp -> L.exp
   val rems  : ty -> L.exp * L.exp -> L.exp
   val remu  : ty -> L.exp * L.exp -> L.exp

   val andb  : ty -> L.exp * L.exp -> L.exp
   val orb   : ty -> L.exp * L.exp -> L.exp
   val xorb  : ty -> L.exp * L.exp -> L.exp
   val eqvb  : ty -> L.exp * L.exp -> L.exp
   val notb  : ty -> L.exp -> L.exp
   val <<    : ty -> L.exp * L.exp -> L.exp
   val >>    : ty -> L.exp * L.exp -> L.exp
   val ~>>   : ty -> L.exp * L.exp -> L.exp  

   (* Trapping operators *)
   val addt  : ty -> L.exp * L.exp -> L.exp
   val subt  : ty -> L.exp * L.exp -> L.exp
   val mult  : ty -> L.exp * L.exp -> L.exp
   val divt  : ty -> L.exp * L.exp -> L.exp
   val remt  : ty -> L.exp * L.exp -> L.exp

   val cond  : ty -> L.cond * L.exp * L.exp -> L.exp

   (* Integer comparisons *)
   val ==    : ty -> L.exp * L.exp -> L.cond
   val <>    : ty -> L.exp * L.exp -> L.cond
   val >     : ty -> L.exp * L.exp -> L.cond
   val <     : ty -> L.exp * L.exp -> L.cond
   val <=    : ty -> L.exp * L.exp -> L.cond
   val >=    : ty -> L.exp * L.exp -> L.cond
   val ltu   : ty -> L.exp * L.exp -> L.cond
   val leu   : ty -> L.exp * L.exp -> L.cond
   val gtu   : ty -> L.exp * L.exp -> L.cond
   val geu   : ty -> L.exp * L.exp -> L.cond

   (* Floating point operators *)
   val fadd  : ty -> L.exp * L.exp -> L.exp
   val fsub  : ty -> L.exp * L.exp -> L.exp
   val fmul  : ty -> L.exp * L.exp -> L.exp
   val fdiv  : ty -> L.exp * L.exp -> L.exp
   val fabs  : ty -> L.exp -> L.exp
   val fneg  : ty -> L.exp -> L.exp
   val fsqrt : ty -> L.exp -> L.exp

   (* Floating point comparisons *)
   val |?|     : ty -> L.exp * L.exp -> L.cond  
   val |!<=>|  : ty -> L.exp * L.exp -> L.cond 
   val |==|    : ty -> L.exp * L.exp -> L.cond 
   val |?=|    : ty -> L.exp * L.exp -> L.cond 
   val |!<>|   : ty -> L.exp * L.exp -> L.cond 
   val |!?>=|  : ty -> L.exp * L.exp -> L.cond 
   val |<|     : ty -> L.exp * L.exp -> L.cond 
   val |?<|    : ty -> L.exp * L.exp -> L.cond 
   val |!>=|   : ty -> L.exp * L.exp -> L.cond 
   val |!?>|   : ty -> L.exp * L.exp -> L.cond
   val |<=|    : ty -> L.exp * L.exp -> L.cond 
   val |?<=|   : ty -> L.exp * L.exp -> L.cond 
   val |!>|    : ty -> L.exp * L.exp -> L.cond 
   val |!?<=|  : ty -> L.exp * L.exp -> L.cond 
   val |>|     : ty -> L.exp * L.exp -> L.cond 
   val |?>|    : ty -> L.exp * L.exp -> L.cond 
   val |!<=|   : ty -> L.exp * L.exp -> L.cond 
   val |!?<|   : ty -> L.exp * L.exp -> L.cond 
   val |>=|    : ty -> L.exp * L.exp -> L.cond 
   val |?>=|   : ty -> L.exp * L.exp -> L.cond
   val |!<|    : ty -> L.exp * L.exp -> L.cond 
   val |!?=|   : ty -> L.exp * L.exp -> L.cond 
   val |<>|    : ty -> L.exp * L.exp -> L.cond 
   val |!=|    : ty -> L.exp * L.exp -> L.cond 
   val |!?|    : ty -> L.exp * L.exp -> L.cond
   val |<=>|   : ty -> L.exp * L.exp -> L.cond 
   val |?<>|   : ty -> L.exp * L.exp -> L.cond

   (* Action combinators *)
   val ||    : L.stm * L.stm -> L.stm   (* parallel L.stms *)
   val Nop   : L.stm                    (* empty L.stm *)
   val Jmp   : int -> L.exp -> L.stm    (* jump to address *)
   val Call  : int -> L.exp -> L.stm    (* call address *)
   val Ret   : L.stm                    (* return *)
   val If    : L.cond * L.stm * L.stm -> L.stm (* if/then/else *)
end
