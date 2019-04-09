(*
 * analysis/group-dag.sig:
 *   Compute the portion of the global dependency graph (a DAG) which
 *   is induced by a single group or library.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature GROUP_DAG = sig

    structure ModuleName: MODULE_NAME
    structure MD: MODDECL
    structure IE: IMP_EXP
    structure SmlSource: SML_SOURCE
    sharing MD = IE.MD and ModuleName = IE.ModuleName = MD.ModuleName
    sharing SmlSource.Compiler = ModuleName.Compiler

    exception MultipleDefinitions of string * string * string
    and Cycle of SmlSource.t * (SmlSource.t * ModuleName.t) list
    and IllegalToplevelOpen of string
    and GroupDagInternalError

    datatype 'ext_info dag =
	DAG of {
		seq_no: int,		(* to speed up merging of dag lists *)
		marked: bool ref,	(* a general purpose mark bit *)
		smlsource: SmlSource.t,
		symmap: ModuleName.t -> IE.env, (* meaning for exp. symbols *)
		intern: 'ext_info dag Set.set,
		extern: 'ext_info
	       }


    (* given:
     *   - a list of Sml sources
     *   - empty external information
     *   - a lookup function for external names (returning and env and info)
     *   - a function to combine pieces of info
     *   - a list of module names, which are used to filter the
     *     export list ([] means no filtering)
     * compute:
     *   - list of names exported by this set of sources (not including the
     *     things re-exported from sub-entities) together with the dag
     *     nodes corresponding to the defining source of each name
     *)

    val analyze:
	{
	 union_dag: 'info dag Set.set * 'info dag Set.set -> 'info dag Set.set,
	 smlsources: SmlSource.t list,
	 enone: 'info,
	 eglob: ModuleName.t -> IE.env * 'info,
	 ecombine: 'info * 'info -> 'info,
	 seq_no: int ref
	}
	->
	(ModuleName.set * 'info dag) list

end
