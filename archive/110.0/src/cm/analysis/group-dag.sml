(*
 * analysis/group-dag.sml:
 *   Compute the portion of the global dependency graph (a DAG) which
 *   is induced by a single group or library.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor GroupDagFun (structure IE: IMP_EXP
		     structure SmlSource: SML_SOURCE
		     sharing IE.MD = SmlSource.MD
		     sharing
			 IE.ModuleName.Compiler =
			 SmlSource.Compiler): GROUP_DAG =
  struct

    structure IE = IE
    structure MD = IE.MD
    structure ModuleName = IE.ModuleName
    structure SmlSource = SmlSource

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

    datatype 'ext_info state =
	UNKNOWN
      | LOCKED
      | KNOWN of 'ext_info dag

    and 'ext_info node =
	N of {
	      smlsource: SmlSource.t,
	      dcl: MD.decl,
	      exports: ModuleName.set,
	      state: 'ext_info state ref
	     }

    and 'ext_info info =
	INFO of 'ext_info dag Set.set * 'ext_info

    fun analyze { union_dag,
		  smlsources = sl, enone, eglob, ecombine, seq_no } = let

	val inone = INFO (Set.empty, enone)

	fun icombine (INFO (l1, e1), INFO (l2, e2)) =
	    INFO (union_dag (l1, l2), ecombine (e1, e2))

	(* defsite_of: ModuleName.t * 'info node list -> 'info node option *)
	fun defsite_of (x, []) = NONE
	  | defsite_of (x, (hd as N { exports, ... }) :: tl) =
	    if ModuleName.memberOf exports x then
		SOME hd
	    else
		defsite_of (x, tl)

	(* parse sources, create a list of 'info nodes;
	 * make sure that no name has more than one definition *)
	fun s2n (smlsource, r) = let

	    fun mknode s = let
		val dcl = SmlSource.decl s
	    in
		N {
		   smlsource = s, dcl = dcl,
		   exports = IE.exports dcl
		     handle IE.IllegalToplevelOpen =>
			 raise IllegalToplevelOpen (SmlSource.makestring s),
		   state = ref UNKNOWN
		  }
	    end

	    fun add_node (n as N { exports, smlsource = s1, ...}, r) = let
		val _ =
		    ModuleName.fold (fn (x, ()) =>
				     case defsite_of (x, r) of
					 NONE => ()
				       | SOME (N { smlsource = s2, ... }) =>
					     raise MultipleDefinitions
						 (ModuleName.makestring x,
						  SmlSource.makestring s1,
						  SmlSource.makestring s2))
		    () exports
	    in
		n :: r
	    end
	in
	    add_node (mknode smlsource, r)
	end

	fun process_nodelist nodelist = let

	    exception Cyc of SmlSource.t * (SmlSource.t * ModuleName.t) list

	    fun process (N { smlsource, dcl, exports, state }) =
		case !state of
		    KNOWN dag => dag
		  | LOCKED => raise Cyc (smlsource, [])
		  | UNKNOWN => let
			val _ = (state := LOCKED)
			val (symmap, INFO (dagl, ei), _) =
			    imp state (dcl, SmlSource.makestring smlsource)
			val sno = !seq_no
			val _ = (seq_no := sno + 1)
			val dag = DAG { seq_no = sno,
				        smlsource = smlsource,
					marked = ref true,
					symmap = symmap,
					intern = dagl,
					extern = ei }
			val _ = (state := KNOWN dag)
		    in
			dag
		    end

	    (* iglob:
	     *  id -> ModuleName.t -> IE.env * ('source, 'ext_info) info *)
	    and iglob s name = let
		fun use_eglob () = let
		    val (e, ei) = eglob name
		in
		    (e, INFO (Set.empty, ei))
		end
	    in
		case defsite_of (name, nodelist) of
		    NONE => use_eglob ()
		  | SOME (n as N { state, smlsource = src, ... }) =>
			if state = s then
			    use_eglob () (* hack to break length=1 cycles *)
			else let
			    val (dag as DAG { symmap, ... }) =
				process n
				  handle
				  Cyc (src', []) =>
				      raise Cyc (src', [(src, name)])
				| Cyc (src', l) =>
				      if SmlSource.eq (src', src) then
					  raise Cycle (src, l)
				      else
					  raise Cyc (src', (src, name) :: l)
			in
			    (symmap name, INFO (Set.singleton dag, enone))
			end
	    end

	    and imp s (dcl, sn) =
		IE.imports (dcl, inone, iglob s, icombine, sn)

	    fun mn_map nl = let
		fun gen (N { exports, state = ref (KNOWN dag), ... }, r) =
		    (exports, dag) :: r
		  | gen _ = raise GroupDagInternalError
	    in
		foldl gen [] nl
	    end

	    fun touch (n as N { smlsource, ... }) =
		ignore (process n)
		handle Cyc (_, l) => raise Cycle (smlsource, l)

	in
	    app touch nodelist;
	    mn_map nodelist
	end
    in
	process_nodelist (foldr s2n [] sl)
    end
	      

end
