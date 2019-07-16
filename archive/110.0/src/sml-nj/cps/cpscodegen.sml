(* cpscodegen.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories 
 *
 *) 

functor CPScodeGenerator(
    structure Gen: CPSGEN
    val collect: unit -> Word8Vector.vector
  ) : CODEGENERATOR = struct

    val architecture = Gen.MachSpec.architecture

    structure CPScomp = CPScomp (
	structure CPSgen = Gen
	val collect = collect)

    type csegments = CPScomp.csegments

    fun codegen({error, anyErrors, errorMatch}, lambda) = CPScomp.compile (
	    lambda,
	    NONE,
	    fn severity => fn s => 
		error (0,0) severity
		  (concat["Real constant out of range: ",s,"\n"]))

  end


(*
 * $Log: cpscodegen.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:30  george
 *   Version 109.24
 *
 *)
