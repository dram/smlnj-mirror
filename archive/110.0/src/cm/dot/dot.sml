(*
 * dot/dot.sml: generate input for DOT (draw the dependency graph)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor DotFun (DagTraversal: DAG_TRAVERSAL): DOT =

struct

    structure SysDag = DagTraversal.SysDag
    structure GroupDag = SysDag.GroupDag
    structure ModuleName = SysDag.ModuleName
    structure SmlSource = GroupDag.SmlSource

    (* generate DOT input from analyzed root entity... *)
    fun dot (pr, ae) = let

	val print = pr
	fun print_int (i: int) = print (Int.toString i)
	fun ce2dag (SysDag.CE { dag, ... }) = dag

	val SysDag.AE { roots, ... } = ae

	fun psno sno = (print "n"; print_int sno)

	fun seen _ = ()
	fun e _ = ()
	fun c (_, _) = ()
	fun i (_, _) = ()

	fun pre dag = let
	    val GroupDag.DAG { seq_no, smlsource, ... } = dag
	    val label = SmlSource.makestring smlsource
	    val llen = String.size label
	    val ext = if llen > 4 then
		String.substring (label, llen - 4, 4)
		      else ""
	    val shape = (case ext of
			 ".sig" => "ellipse"
		       | ".sml" => "box"
		       | _ => "diamond")
	in
	    psno seq_no;
	    print (" [shape = " ^ shape ^ ", label = \"" ^ label ^ "\"]\n")
	end

	fun com (_, _, _, _, dag) = let
	    val GroupDag.DAG { seq_no, intern,
			       extern = SysDag.INFO { cross, extern },
			       ... } = dag

	    fun edges style edge_to [] = ()
	      | edges style edge_to l = let
		    fun pedges [] = ()
		      | pedges [only] = edge_to only
		      | pedges (h :: t) = (edge_to h; print "; "; pedges t)
		in
		    psno seq_no;
		    print " -> { ";
		    pedges l;
		    print (" }" ^ style ^ "\n")
		end

	    fun int_edge_to (GroupDag.DAG { seq_no = n, ... }) = psno n
	    fun ext_edge_to mn = 
		print ("\"" ^ (ModuleName.makestring mn) ^ "\"")

	    fun int_edges x =
		edges "" int_edge_to x
	    val ext_edges =
		edges " [ style = dotted ]" ext_edge_to
	    val cross_edges =
		edges " [ style = dashed ]" (int_edge_to o ce2dag)
	in
	    int_edges (Set.makelist intern);
	    cross_edges (Set.makelist cross);
	    ext_edges (ModuleName.makelist extern)
	end

    in
	print
	"digraph G {\npage = \"8.5,11\";\nsize = \"7.5,10\";\nratio = fill;\n\
\concentrate = true;\nnode [shape = plaintext, fontsize = 34];\n";
	DagTraversal.traversal { seen_before = seen, pre = pre, extern = e,
				 cross = c, intern = i, combine = com } roots;
	print "}\n"
    end

end
