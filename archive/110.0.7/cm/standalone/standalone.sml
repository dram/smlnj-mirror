(*
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor SAloneFun (DagTraversal: DAG_TRAVERSAL): SALONE = struct

    structure SysDag = DagTraversal.SysDag
    structure GroupDag = SysDag.GroupDag
    structure ModuleName = SysDag.ModuleName
    structure SmlSource = GroupDag.SmlSource

    type fname = AbsPath.t

    datatype env =
	PERVASIVE
      | ORIGINAL of int
      | DERIVED of int

    fun standAlone (pr, ae) = let

	val SysDag.AE { roots, namemap, ... } = ae

	local
	    val nenv = ref 0
	in
	    fun newDEnv () = let
		val e = !nenv
		val _ = nenv := e + 1
	    in
		DERIVED e
	    end
	end

	local
	    fun prset s = let
		fun prsym s = let
		    val ns = ModuleName.namespaceOf s
		    val n = ModuleName.nameOf s
		in
		    pr (if ns = ModuleName.STRspace then "STR"
			else if ns = ModuleName.SIGspace then "SIG"
			     else if ns = ModuleName.FCTspace then "FCT"
				  else "FSIG");
		    pr (concat [" \"", n, "\""])
		end
		fun loop [] = ()
		  | loop [s] = prsym s
		  | loop (h :: t) = (prsym h; pr ", "; loop t)
	    in
		loop (ModuleName.makelist s)
	    end
	in
	    fun fname f = "f" ^ (Int.toString f)
	    fun ename (ORIGINAL e) = "o" ^ (Int.toString e)
	      | ename (DERIVED e) = "d" ^ (Int.toString e)
	      | ename PERVASIVE = "pervenv"

	    local
		val filtt: (ModuleName.set, int) Table.table =
		    Table.create ModuleName.sameSet
		val nfilt = ref 0
	    in
		fun MkFilt set =
		    case Table.find (filtt, set) of
			SOME f => f
		      | NONE => let
			    val f = !nfilt
			    val _ = nfilt := f + 1
			    val _ = Table.enter (filtt, set, f)
			in
			    pr (concat ["val ", fname f, " = filter ["]);
			    prset set;
			    pr "]\n";
			    f
			end
	    end

	    local
		fun fe_eq ((f: int, e: env), (f', e')) = f = f' andalso e = e'
		val fetab: ((int * env), env) Table.table = Table.create fe_eq
	    in
		fun Filter { filter, arg } =
		    case Table.find (fetab, (filter, arg)) of
			SOME res => res
		      | NONE => let
			    val res = newDEnv ()
			    val _ = Table.enter (fetab, (filter, arg), res)
			in
			    pr (concat ["val ", ename res, " = ",
					fname filter, " ",
					ename arg, "\n"]);
			    res
			end
	    end

	    local
		fun eql ([], []) = true
		  | eql (h :: t, h' :: t') = h = h' andalso eql (t, t')
		  | eql _ = false
		val ltab: (env list, env) Table.table = Table.create eql
	    in
		fun Layer [e] = e
		  | Layer args =
		    case Table.find (ltab, args) of
			SOME res => res
		      | NONE => let
			    val res = newDEnv ()
			    val _ = Table.enter (ltab, args, res)
			    fun prenv e = pr (ename e)
			    fun prenvs [] = ()
			      | prenvs [e] = prenv e
			      | prenvs (h :: t) = (prenv h; pr ", "; prenvs t)
			in
			    pr (concat ["val ", ename res, " = layer ["]);
			    prenvs args;
			    pr "]\n";
			    res
			end
	    end

	    fun Run { source, env, res } =
		pr (concat ["val ", ename res, " = run (\"", AbsPath.elab source, "\", ", ename env, ")\n"])
	end

	fun pre _ = ()
	fun seen_before (GroupDag.DAG { seq_no, ... }) = ORIGINAL seq_no
	fun extern _ = ()

	fun cross (SysDag.CE { filter = NONE, ... }, e) = e
	  | cross (SysDag.CE { filter = SOME filter, ... }, e) =
	    Filter { filter = MkFilt filter, arg = e}

	fun intern (_, e) = e

	fun combine (_, _, cl, il, d) = let
	    val GroupDag.DAG { seq_no, smlsource, ... } = d
	    val e0 = Layer (il @ cl @ [PERVASIVE])
	    val res = ORIGINAL seq_no
	    val s = SmlSource.name smlsource
	    val _ = Run { source = s, env = e0, res = res }
	in
	    res
	end

	val _ = pr ("local open Compiler.CMSA val pervenv = pervenv ()\n");

	val el = DagTraversal.traversal { seen_before = seen_before, pre = pre,
					  extern = extern, cross = cross,
					  intern = intern, combine = combine }
				roots
	fun get1export (_, ce as SysDag.CE { dag = GroupDag.DAG { seq_no,... },
					     ... }) =
	    cross (ce, ORIGINAL seq_no)
	val finenv = Layer (map get1export namemap)
    in
	pr (concat ["in val _ = register ", ename finenv, " end\n"])
    end

end
