(* This testcase propagates an ERRORent deep into the 
 * matchDefStr0 checking code.
 *
 * Derived from amd64-gen-fn.sml, ...
 * 
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Glues together the AMD64-specific code with the code generator.  Also
 * specializes register spilling.
 *)


signature MLTREESIG = sig end (* MLTREE *)

signature CSIG = sig end (* C_CALL *)

signature MLRTS = sig structure T : MLTREESIG end (* MLRTS *)

signature TSIG = sig structure MTy : MLRTS end (* TRANSFER *)

functor HeapTransferFn (
    structure MTy : MLRTS
    structure CCall : CSIG
	where T = MTy.T
) = struct
  structure MTy = MTy 
  structure T = MTy.T 
end (* HeapTransferFn *)

signature BS = 
sig
    structure MTy : MLRTS
    structure Transfer : TSIG
	where MTy = MTy
end (* BS*)

functor MLRT ( structure T : MLTREESIG ) : MLRTS = 
struct
  structure T = T  
end (* MLRT *)

structure AMLTree = struct end

functor AMD64GenFn () = 
struct
  structure BackEnd : BS = 
  struct
    structure MTy = MLRT (structure T = AMLTree ) 

    structure Transfer = 
        HeapTransferFn (structure MTy =MTy
                        structure CCall = MissingFn (structure T=AMLTree))

  end (* BackEnd *)
  
end (* AMD64CG *)
