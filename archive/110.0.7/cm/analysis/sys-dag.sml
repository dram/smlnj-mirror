(*
 * analysis/sys-dag.sml:
 *   Compute the global dependency graph (a DAG) for the entire system.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor SysDagFun (structure GroupDag: GROUP_DAG
		   structure EntityDesc: ENTITY_DESC
		   structure Decl: DECL
		   structure FnameRules: FNAME_RULES
		   structure Control: CONTROL
		   sharing
		       Decl.MD = GroupDag.SmlSource.MD): SYS_DAG =
  struct

    structure GroupDag = GroupDag
    structure ModuleName = GroupDag.ModuleName
    structure MN = ModuleName
    structure IE = GroupDag.IE
    structure EntityDesc = EntityDesc
    structure Tools = EntityDesc.Tools
    structure SmlSource = GroupDag.SmlSource
    structure Compiler = MN.Compiler

    exception EntityCycle of AbsPath.t list
    and MultipleDefinitions of string list * string * string * string
    and ExportedNamesNotDefined of string list * string
    and Stabilize of string
    and SysDagInternalError
    and MultiMember of { m: string, d: string, d': string option }

    type pid = Compiler.PersStamps.persstamp

    datatype crossedge =
	CE of { dag: dag, filter: MN.set option, lib: bool,
	        pidpair: { orig: pid, filtered: pid } option ref }
    and info =
	INFO of { cross: ceset, extern: MN.set }
    withtype dag = info GroupDag.dag
    and ceset = crossedge Set.set

    datatype desc =
	CMFILE of AbsPath.t
      | SCGROUP of AbsPath.t
      | SCLIBRARY of AbsPath.t

    datatype analyzed_entity =
	AE of {
	       roots: dag Set.set,
	       namemap: (MN.set * crossedge) list,
	       lib: bool,
	       stabilizer: (desc -> unit) -> bool -> unit
	      }

    fun eq_dag (GroupDag.DAG { seq_no = s1, ... }: dag,
		GroupDag.DAG { seq_no = s2, ... }) = s1 = s2
    fun lt_dag (GroupDag.DAG { seq_no = s1, ... }: dag,
		GroupDag.DAG { seq_no = s2, ... }) = s1 < s2

    fun eq_ce (CE { dag = d1, ... },
	       CE { dag = d2, ... }) = eq_dag (d1, d2)
    fun lt_ce (CE { dag = d1, ... },
	       CE { dag = d2, ... }) = lt_dag (d1, d2)

    val { union = union_dag, addl = addl_dag, makeset = makeset_dag, ... } =
	Set.gen { eq = eq_dag, lt = lt_dag }
    val { union = union_ce, ... } =
	Set.gen { eq = eq_ce, lt = lt_ce }
	 
    val none = INFO { cross = Set.empty, extern = MN.empty }
    fun combine (INFO { cross = c1, extern = e1 },
		 INFO { cross = c2, extern = e2 }) =
	INFO { cross = union_ce (c1, c2),
	       extern = MN.union (e1, e2) }

    fun lookup (_, []) = NONE
      | lookup (x, (s, v) :: t) =
	if MN.memberOf s x then
	    SOME v
	else
	    lookup (x, t)

    fun reset_marks roots = let
	fun sub_reset (GroupDag.DAG { intern, extern, ... }) = let
	    val INFO { cross, extern } = extern
	    fun reset_ce (CE { dag, ... }, ()) = reset dag
	in
	    Set.fold reset_ce () cross; app reset (Set.makelist intern)
	end
	and reset (GroupDag.DAG { marked = ref false, ... }) = ()
	  | reset (dag as GroupDag.DAG { marked, ... }) =
	    (marked := false; sub_reset dag)
    in
	app sub_reset roots
    end

    fun get_entity ep (CMFILE f) = EntityDesc.read ep f
      | get_entity ep (SCGROUP f) = EntityDesc.readSCGroup ep f
      | get_entity ep (SCLIBRARY f) = EntityDesc.readSCLibrary ep f

    fun mkname (EntityDesc.SIG s) = MN.sigMN s
      | mkname (EntityDesc.STR s) = MN.structMN s
      | mkname (EntityDesc.FCT s) = MN.functMN s
      | mkname (EntityDesc.FSIG s) = MN.funsigMN s

    fun sameFile (f, f') = AbsPath.compare (f, f') = EQUAL

    fun analyze (ep, rootdesc, staticBase) = let

	val descTable = Table.create sameFile

	fun checkMember d m =
	    case Table.find (descTable, m) of
		SOME d' => let
		    val d' =
			if AbsPath.compare (d, d') = EQUAL then NONE
			else SOME (AbsPath.elab d')
		in
		    raise MultiMember { m = AbsPath.elab m,
				        d = AbsPath.elab d,
					d' = d' }
		end
	      | NONE => Table.enter (descTable, m, d)

	val seq_no = ref 0

	val blookup =
	    IE.mkBaseLookup (Compiler.EnvRef.unSCstaticEnv staticBase)

	val get_entity = get_entity ep

	fun hist h = let
	    fun spaces ([], l) = l
	      | spaces ([x], l) = x :: l
	      | spaces (x :: y, l) = spaces (y, " " :: x :: l)
	in
	    case h of
		[] => NONE
	      | _ => SOME (concat ("via: " :: spaces (h, [])))
	end

	fun same_crossedge ((s1: int, f1, l1: bool), (s2, f2, l2)) = let
	    fun samef (NONE, NONE) = true
	      | samef (SOME f1, SOME f2) = let
		    fun loop ([], []) = true
		      | loop (h1 :: t1, h2 :: t2) =
			MN.equal (h1, h2) andalso loop (t1, t2)
		      | loop _ = false
		in
		    loop (MN.makelist f1, MN.makelist f2)
		end
	      | samef _ = false
	in
	    s1 = s2 andalso l1 = l2 andalso samef (f1, f2)
	end

	val crossedge_table:
	      (int * MN.set option * bool, crossedge) Table.table =
	      Table.create same_crossedge

	fun new_ce { dag as GroupDag.DAG { seq_no, ... }, filter, lib } = let
	    val key = (seq_no, filter, lib)
	in
	    case Table.find (crossedge_table, key) of
		NONE => let
		    val ce = CE { dag = dag, filter = filter, lib = lib,
				  pidpair = ref NONE }
		in
		    Table.enter (crossedge_table, key, ce);
		    ce
		end
	      | SOME ce => ce
	end

	val entity_table: (AbsPath.t, analyzed_entity option) Table.table =
	    Table.create sameFile

	exception Cyc of AbsPath.t * AbsPath.t list

	fun get_analyzed_entity (isroot, desc) = let
	    val e as EntityDesc.ENTITY { location, ... } = get_entity desc
	in
	    case Table.find (entity_table, location) of
		NONE => let
		    val _ = Table.enter (entity_table, location, NONE)
		    val ae = analyze_entity (isroot, e, desc)
			handle Cyc (location', l) =>
			    if sameFile (location, location') then
				raise EntityCycle l
			    else
				raise Cyc (location', location :: l)
		    val _ = Table.enter (entity_table, location, SOME ae)
		in
		    ae
		end
	      | SOME NONE => raise Cyc (location, [location])
	      | SOME (SOME ae) => ae
	end

	and analyze_entity (isroot, e, desc) = let

	    val EntityDesc.ENTITY { lib = islib, exports = expfilter,
				    members, stable, location } = e

	    fun separate eml = let
		fun loop ([], sl, el) = (sl, el)
		  | loop ((EntityDesc.M mspec) :: t, sl, el) = let
			val { name, history, classification } = mspec
		    in
			case classification of
			    Tools.CMFILE => loop (t, sl, (CMFILE name) :: el)
			  | Tools.SCGROUP => loop (t, sl, (SCGROUP name) :: el)
			  | Tools.SCLIBRARY =>
				loop (t, sl, (SCLIBRARY name) :: el)
			  | Tools.SMLSOURCE =>
				(checkMember location name;
				 loop (t, SmlSource.get { file = name,
							  creator =
							        hist history }
				       :: sl, el))
			  | Tools.TOOLINPUT { targets, validate, make } => let
				val _ = if not (validate ()) then make ()
					else ()
				fun c ((n, co), r) = let
				in
				    EntityDesc.M { name = n,
						   history =
						        AbsPath.elab name ::
							history,
						    classification =
					               Tools.classify (n, co) }
				    :: r
				end
			    in
				loop (foldr c t targets, sl, el)
			    end
		    end
	    in
		loop (eml, [], [])
	    end

	    val islib = if isroot then false else islib
	    val (sl, el) = separate members

	    val filternames = MN.makeset (map mkname expfilter)
	    val no_filter = MN.isEmpty filternames
	    val expfilter = if no_filter then NONE else SOME filternames

	    fun union_filter (NONE, _) = NONE
	      | union_filter (_, NONE) = NONE
	      | union_filter (SOME s1, SOME s2) =
		SOME (MN.union (s1, s2))

	    fun intersect_filter (NONE, f2) = f2
	      | intersect_filter (f1, NONE) = f1
	      | intersect_filter (SOME s1, SOME s2) =
		SOME (MN.intersection (s1, s2))

	    fun add_nm (x as (s, ce), l) = let

		val CE { dag = d as GroupDag.DAG { smlsource = sr,
						   ... },
			 filter = f, ... } = ce

		fun check (s', CE { dag = GroupDag.DAG { smlsource = sr',
							 ... }, ... }) =
		    let
			val inter = MN.intersection (s, s')
		    in
			if MN.isEmpty inter then ()
			else let
			    val names = map MN.makestring (MN.makelist inter)
			in
			    raise MultipleDefinitions
				(names, AbsPath.elab location,
				 SmlSource.makestring sr,
				 SmlSource.makestring sr')
			end
		    end

		fun a [] = [x]
		  | a ((h as (s', ce')) :: t) = let
			val CE { dag = d', filter = f', ... } = ce'
		    in
			if eq_dag (d, d') then let
			    val _ = app check t
			    val ns = MN.union (s, s')
			    val nf = union_filter (f, f')
			    val nce =
				new_ce { dag = d, filter = nf, lib = islib }
			in
			    (ns, nce) :: t
			end
			else
			    (check h; h :: (a t))
		    end
	    in
		a l
	    end

	    fun masking_add_nm (x as (s, _), l) = let

		fun sub ((s', ce'), r) = let
		    val diff = MN.difference (s', s)
		in
		    if MN.isEmpty diff then r
		    else (diff, ce') :: r
		end

	    in
		foldl sub [x] l
	    end

	    fun add_single (n, ce, []) = [(MN.singleton n, ce)]
	      | add_single (n, ce as CE { dag = d, ... },
			    (h as (s, ce' as CE { dag = d', ... })) :: t) =
		if eq_dag (d, d') then
		    (MN.add (n, s), ce) :: t
		else
		    h :: add_single (n, ce, t)

	    fun filter_ce (CE { dag, filter, lib, ... }) = let
		val newfilt = intersect_filter (expfilter, filter)
		val ce = new_ce { dag = dag, filter = newfilt, lib = lib }
	    in
		case newfilt of
		    NONE => SOME ce
		  | SOME s => 
			if MN.isEmpty s then
			    NONE
			else
			    SOME ce
	    end

	    fun gae (e, (r, snm, senm, stab)) = let
		val AE { roots, namemap, lib, stabilizer, ... } =
		    get_analyzed_entity (false, e)
	    in
		(union_dag (r, roots),
		 foldl add_nm snm namemap,
		 if lib then senm else foldl add_nm senm namemap,
		 fn recomp => (stabilizer recomp true; stab recomp))
	    end

	    val (sub_roots, sub_namemap, exported_sub_namemap, sub_stab) =
		foldr gae (Set.empty, [], [], fn recomp => ()) el

	    val stabilizer =
		if not stable then let

		    val done = ref false

		    fun sml2mem (sml, r) = let
			val name = SmlSource.name sml
			val bf = FnameRules.binFileFor name
			val _ = AbsPath.exists bf orelse
			    raise Stabilize (AbsPath.elab bf)
			val decl = SmlSource.decl sml
		    in
			(Decl.FILE { name = name, decl = decl }) :: r
		    end

		    fun e2mem (CMFILE f) =
			Decl.ENTITY { name = f, class = "cmfile" }
		      | e2mem (SCGROUP f) =
			Decl.ENTITY { name = f, class = "scgroup" }
		      | e2mem (SCLIBRARY f) =
			Decl.ENTITY { name = f, class = "sclibrary" }

		    val stablefile = FnameRules.stableFileFor location
		    fun stabilizer recomp recursive =
			if !done then ()
			else let
			    val _ = done := true
			in
			    if recursive then sub_stab recomp else ();
			    recomp desc;
			    Decl.create_stable
			      (foldr sml2mem (map e2mem el) sl, stablefile)
			end
		in
		    stabilizer
		end
		else fn _ => fn _ => ()

	    fun glob name =
		case lookup (name, sub_namemap) of
		    NONE => (blookup name,
			     INFO { cross = Set.empty,
				    extern = MN.singleton name })
		  | SOME (ce as CE { dag = GroupDag.DAG { symmap, ... },
				     ... }) =>
		    (symmap name, INFO { cross = Set.singleton ce,
					 extern = MN.empty })

	    val gnamemap =
		GroupDag.analyze { union_dag = union_dag,
				   smlsources = sl, enone = none,
				   eglob = glob, ecombine = combine,
				   seq_no = seq_no }

	    fun mk_ce dag =
		new_ce { dag = dag, filter = expfilter, lib = islib }

	    val add_ce =
		if no_filter then
		    fn ((s, dag), (l, r)) => ((s, mk_ce dag) :: l, r)
		else let
		    fun a_ce ((s, dag), (l, r)) = let
			val s' = MN.intersection (s, filternames)
		    in
			if MN.isEmpty s' then
			    (l, dag :: r)
			else
			    ((s', mk_ce dag) :: l, r)
		    end
		in
		    a_ce
		end

	    fun assoc_ce (s, dag) = (s, mk_ce dag)

	    val (namemap, dummies) = List.foldl add_ce ([], []) gnamemap

	    (*
	     * If the filter-list is empty (which cannot happen in the case of
	     * libraries), then we take all names except for the shadowed ones.
	     * Otherwise we *only* take things from the filter-list.
	     *)
	    val newnamemap =
		if no_filter then
		    foldr masking_add_nm exported_sub_namemap namemap
		else let
		    fun get (n, (good, bad)) =
			case lookup (n, namemap) of
			    SOME ce => (add_single (n, ce, good), bad)
			  | NONE =>
				(case lookup (n, sub_namemap) of
				     SOME ce =>
					 (case filter_ce ce of
					      NONE =>
						  raise SysDagInternalError
					    | SOME ce =>
						  (add_single (n, ce, good),
						   bad))
				   | NONE => (good, n :: bad))
		in
		    case MN.fold get ([], []) filternames of
			(good, []) => good
		      | (_, bad) => raise ExportedNamesNotDefined
			    (map MN.makestring bad, AbsPath.elab location)
		end

	    fun get_dag (_, CE { dag, ... }) = dag

	    (*
	     * In the case of libraries we simply discard the set of roots:
	     * library files are only loaded on demand.
	     *)
	    val roots =
		if islib then Set.empty
		else addl_dag (dummies,
			       addl_dag (map get_dag newnamemap,
					 sub_roots))

	    val _ =
		if Control.show_exports NONE andalso no_filter then let
		    fun show_names (s, _) = let
			fun show_name (n, ()) =
			    Control.say (concat ["\t",
						 MN.makestring n, "\n"])
		    in
			MN.fold show_name () s
		    end
		in
		    Control.say (concat [if islib then "Library" else "Group",
					 " (* ",
					 AbsPath.elab location,
					 " *)\n"]);
		    app show_names newnamemap
		end
		else ()

	in
	    AE { roots = roots, namemap = newnamemap, lib = islib,
		 stabilizer = stabilizer }
	end

	fun is_marked (GroupDag.DAG { marked = ref m, ... }) = m

	val AE { roots, namemap, lib, stabilizer } =
	    get_analyzed_entity (true, rootdesc)
	    handle Cyc (_, l) => raise SysDagInternalError

	val _ = reset_marks (Set.makelist roots)

	val roots = Set.filter is_marked roots

	fun demand (_, CE { dag = GroupDag.DAG { marked, ... }, ... }) = 
	    marked := false
	fun undemand (GroupDag.DAG { marked, ... }) = marked := true

	val _ = app demand namemap
	val _ = case Set.makelist (Set.filter is_marked roots) of
	    [] => ()
	  | orphans => let
		fun show (GroupDag.DAG { smlsource, ... }) =
		    Control.say (concat ["\t", SmlSource.makestring smlsource,
					 "\n"])
	    in
		Control.say
		    "!% Warning: units not accessible due to filtering:\n";
		app show orphans
	    end
	val _ = app undemand (Set.makelist roots)

      in
	  AE { roots = roots, namemap = namemap, lib = lib,
	       stabilizer = stabilizer }
      end

    fun select_roots (AE { lib, namemap, ... }, mns) = let
	fun lookup' (_, []) = NONE
	  | lookup' (x, (h as (s, _)) :: t) =
	    if MN.memberOf s x then
		SOME h
	    else
		lookup' (x, t)
	fun isnew (ce, []) = true
	  | isnew (ce, (s, v) :: t) = eq_ce (ce, v) orelse isnew (ce, t)
	fun add'if'new (x as (_, ce), l) = if isnew (ce, l) then x :: l else l
	fun get1 (mn, (resolved, unresolved)) =
	    case lookup' (mn, namemap) of
		NONE => (resolved, MN.add (mn, unresolved))
	      | SOME (x as (s, ce)) => (add'if'new (x, resolved), unresolved)
	val (r, ur) = MN.fold get1 ([], MN.empty) mns
	fun to'dag (_, CE { dag, ... }) = dag
	val roots = makeset_dag (map to'dag r)
    in
	{ ae = case r of
	         [] => NONE
	       | _ => SOME (AE { lib = lib, namemap = r, roots = roots,
				 stabilizer = fn _ => fn _ => () }),
	  unresolved = ur }
    end

    fun pick_root (AE { lib, namemap, ... }, mn) =
	case lookup (mn, namemap) of
	    NONE =>
		raise ExportedNamesNotDefined ([MN.makestring mn],
					       "CMB's pick_root operation")
	  | SOME (ce as CE { dag, ... }) =>
		AE { lib = lib, namemap = [(MN.singleton mn, ce)],
		     roots = Set.singleton dag,
		     stabilizer = fn _ => fn _ => () }

  end
