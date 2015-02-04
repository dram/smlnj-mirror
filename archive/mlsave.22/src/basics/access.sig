(* access.sig *)
signature PSIG = sig
 datatype primop = 
	! | * | + | - | := | < | <= | > | >= |
	alength | boxed | create | div | cast | eql |
	fadd |fdiv |feql |fge |fgt |fle |flt |fmul |fneg |fneq |fsub |
	gethdlr | ieql | ineq | neq | makeref |
	ordof | profile | sethdlr | sceql | slength | store | subscript |
	unboxedassign | unboxedupdate | update | ~
 end

signature ACCESS = sig

  structure Symbol : SYMBOL
  structure P : PSIG
  type primop
  type lvar  (* lambda variable id *)
  type slot  (* position in structure record *)
  type path  (* slot chain relative to lambda variable *)

  datatype access 
    = LVAR of lvar
    | SLOT of slot
    | PATH of path  
    | INLINE of primop
  
  val mkLvar : unit -> lvar
  val sameName : lvar * lvar -> unit
  val dupLvar : lvar -> lvar
  val namedLvar : Symbol.symbol -> lvar
  val lvarName : lvar -> string
  val saveLvarNames : bool ref

end (* signature ACCESS *)
