(* cpsphases.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature REGSPEC =
  sig 
    val maxgpfree : int      (* maximum number of gpr in the target machine *)
    val maxfpfree : int      (* maximum number of fpr in the target machine *)

    val numCSgp : int ref       (* number of gpr calleesave registers *)
    val numCSfp : int ref       (* number of fpr calleesave registers *)

    val repflag : bool ref

    val maxrepregs1 : int ref
    val maxrepregs2 : int ref

    val unboxedfloat : bool ref 
    val untaggedint : bool ref

    val numgp : CPS.cty list -> int    
    val numfp : CPS.cty list -> int

  end

signature ETA = sig
    val eta : {function: CPS.function,
	       click: string -> unit} -> CPS.function
end

signature CPSOPT = sig
    val reduce : (CPS.function * LtyKernel.lty Intmap.intmap 
                  * Unsafe.Object.object option * bool) 
	         -> CPS.function * LtyKernel.lty Intmap.intmap
end

signature CPSTRANS = sig 
    val cpstrans : CPS.function -> CPS.function
end

signature CONVERT = sig 
    val convert : Lambda.lexp -> CPS.function * LtyKernel.lty Intmap.intmap 
end

signature CONTRACT = sig
    val contract : {function: CPS.function,
		    table: LtyKernel.lty Intmap.intmap,
		    click: string -> unit,
		    last: bool,
		    size: int ref}
	            -> CPS.function
end

signature EXPAND = sig
    val expand : {function: CPS.function,
		  bodysize: int,
		  unroll: bool,
		  table: LtyKernel.lty Intmap.intmap,
		  afterClosure: bool, do_headers: bool,
		  click: string -> unit} -> CPS.function
end

signature ETASPLIT =
  sig val etasplit : {function: CPS.function,
		      table: LtyKernel.lty Intmap.intmap,
		      click: string -> unit} -> CPS.function
  end

signature FLATTEN = sig
    val flatten : {function: CPS.function,
		   table: LtyKernel.lty Intmap.intmap,
		   click: string -> unit}
	-> CPS.function
end

signature CLOSURE = sig
    val closeCPS : CPS.function -> CPS.function
end

signature SPILL = sig
   val spill : CPS.function list -> CPS.function list 
end

signature LIMIT = sig
    val nolimit : CPS.function list -> 
	              CPS.function list * (CPS.lvar -> (int * int))
end

signature CPSGEN = sig
    structure MachSpec : MACH_SPEC

    val codegen : CPS.function list * (CPS.lvar -> (int * int))
                  * ErrorMsg.complainer -> unit
end 


(*
 * $Log: cpsphases.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/06/30 19:37:16  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:30  george
 *   Version 109.24
 *
 *)
