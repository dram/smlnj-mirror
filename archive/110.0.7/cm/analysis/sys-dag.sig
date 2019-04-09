(*
 * analysis/sys-dag.sig:
 *   Compute the global dependency graph (a DAG) for the entire system.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SYS_DAG = sig

    structure GroupDag: GROUP_DAG
    structure EntityDesc: ENTITY_DESC
    structure ModuleName: MODULE_NAME
    structure Compiler: COMPILER
    sharing ModuleName.Compiler = GroupDag.SmlSource.Compiler = Compiler
    sharing ModuleName = GroupDag.ModuleName

    exception EntityCycle of AbsPath.t list
    and MultipleDefinitions of string list * string * string * string
    and ExportedNamesNotDefined of string list * string
    and Stabilize of string
    and SysDagInternalError
    and MultiMember of { m: string, d: string, d': string option }

    type pid = Compiler.PersStamps.persstamp

    (*
     * Filtering is a property of edges - not of nodes.
     * Within an entity there is no filtering...
     *)
    datatype crossedge =
	CE of { dag: dag, filter: ModuleName.set option, lib: bool,
	        pidpair: { orig: pid, filtered: pid } option ref }
    and info =
	INFO of { cross: ceset, extern: ModuleName.set }
    withtype dag = info GroupDag.dag
    and ceset = crossedge Set.set

    datatype desc =
	CMFILE of AbsPath.t
      | SCGROUP of AbsPath.t
      | SCLIBRARY of AbsPath.t

    datatype analyzed_entity =
	AE of {
	       roots: dag Set.set,
	       namemap: (ModuleName.set * crossedge) list,
	       lib: bool,
	       stabilizer: (desc -> unit) -> bool -> unit
	      }

    val analyze:
	EntityDesc.elab_params * desc *
	(* GroupDag.IE. *) Compiler.Environment.staticEnv
	->
	analyzed_entity

    (*
     * Thinning of AE's according to a set of names.  The resulting
     * AE will have only those roots which are necessary to define
     * names in the second parameter.  Other names defined by the same
     * roots will still be part of the namemap.
     *)
    val select_roots: analyzed_entity * ModuleName.set ->
	{ ae: analyzed_entity option, unresolved: ModuleName.set }

    (*
     * Special purpose thinning according to precisely one symbol.
     * The resulting AE has exactly one root and the namemap contains
     * only one symbol (the one that was provided for the second argument).
     *)
    val pick_root: analyzed_entity * ModuleName.t -> analyzed_entity

    val reset_marks: dag list -> unit

    val lookup: ModuleName.t * (ModuleName.set * 'a) list -> 'a option

end
