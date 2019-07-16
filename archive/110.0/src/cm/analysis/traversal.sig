(*
 * analysis/traversal.sig:
 *   Generic traversal of dependency DAG.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature DAG_TRAVERSAL = sig

    structure SysDag: SYS_DAG

    (*
     * traversal:
     *   -- specialized by:
     *       + a function to be called if the node has been visited before
     *       + a pre-order visitor function
     *       + a function to gather info from external edges
     *       + a function to gather info from cross edges
     *       + a function to gather info from internal edges
     *       + a function combining the results
     *)
    val traversal:
	{ seen_before: SysDag.dag -> 'r,
	  pre: SysDag.dag -> 'pre,
	  extern: SysDag.ModuleName.t -> 'e,
	  cross: SysDag.crossedge * 'r -> 'c,
	  intern: SysDag.dag * 'r -> 'i,
	  combine: 'pre * 'e list * 'c list * 'i list * SysDag.dag -> 'r
	 }
	->
	SysDag.dag Set.set
	->
	'r list

end
