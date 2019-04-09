(*
 * sched/prune.sml:
 *   Find transitive closure of dependency relation starting from an
 *   `a priory' subset of the sources and prune dependency graph
 *   accordingly.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor PruneFun (Traversal: DAG_TRAVERSAL): PRUNE = struct

    structure SD = Traversal.SysDag
    structure GD = SD.GroupDag

    fun eq_dag (GD.DAG { seq_no = s1, ... }: SD.dag,
		GD.DAG { seq_no = s2, ... }) =
	s1 = s2
    fun lt_dag (GD.DAG { seq_no = s1, ... }: SD.dag,
		GD.DAG { seq_no = s2, ... }) =
	s1 < s2
    fun r_ce dr (SD.CE { dag = d1, ... },
		 SD.CE { dag = d2, ... }) =
	dr (d1, d2)
    val eq_ce = r_ce eq_dag
    val lt_ce = r_ce lt_dag

    val { makeset = makeset_dag, ... } = Set.gen { eq = eq_dag, lt = lt_dag }
    val { makeset = makeset_ce, ... } = Set.gen { eq = eq_ce, lt = lt_ce }

    fun collapse (NONE, l) = l
      | collapse (SOME x, l) = x :: l

    fun prune apriori = let

	val table: (SD.dag, SD.dag) Table.table = Table.create eq_dag
	val seq_no = ref 0

	fun seen_before dag = Table.find (table, dag)

	fun pre _ = ()

	fun extern _ = ()

	fun cross (_, NONE) = NONE
	  | cross (SD.CE { filter, lib, ... }, SOME d) =
	    SOME (SD.CE { dag = d, filter = filter, lib = lib,
			  pidpair = ref NONE })

	fun intern (_, d) = d

	fun combine (_, _, c, i, od as GD.DAG odr) = let
	    val c = foldr collapse [] c
	    val i = foldr collapse [] i
	in
	    if not (null c) orelse not (null i) orelse apriori od then
		let
		    val info = SD.INFO { cross = makeset_ce c,
					 extern = SD.ModuleName.empty }
		    val sno = !seq_no
		    val _ = seq_no := sno + 1
		    val nd = GD.DAG { seq_no = sno,
				      marked = ref false,
				      smlsource = #smlsource odr,
				      symmap = #symmap odr,
				      intern = makeset_dag i,
				      extern = info }
		in
		    Table.enter (table, od, nd);
		    SOME nd
		end
	    else
		NONE
	end
	val trav = Traversal.traversal
	val traverse =
	    trav { seen_before = seen_before, pre = pre, extern = extern,
		  cross = cross, intern = intern, combine = combine }

	fun prun (SD.AE { roots, namemap, lib, ... }) = let
	    val l = foldr collapse [] (traverse roots)
	    fun newnm ((mns, SD.CE { dag, filter, lib, ... }), nm) =
		case Table.find (table, dag) of
		    NONE => nm
		  | SOME d =>
			(mns, SD.CE { dag = d,
				      filter = filter,
				      lib = lib,
				      pidpair = ref NONE }) :: nm
	    val namemap = foldr newnm [] namemap
	    fun newr (r, l) =
		case Table.find (table, r) of
		    NONE => l
		  | SOME (d as GD.DAG { marked, ... }) => (marked := true; d :: l)
	    val roots = makeset_dag (Set.fold newr [] roots)
	in
	    SD.AE { roots = roots, namemap = namemap, lib = lib,
		    stabilizer = fn _ => fn _ => () }
	end
    in
	prun
    end
end
