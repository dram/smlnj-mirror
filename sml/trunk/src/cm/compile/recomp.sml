(*
 * Build an argument for the generic compilation functor.
 *   This gives a traversal that loads from binfiles, stable archives,
 *   or compiles sml source code.  The "binfile content" cache gets
 *   warmed up that way, too.  (The "ExecFn" functor takes advantage of
 *   this fact.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor RecompFn (structure PS : RECOMP_PERSSTATE) : COMPILATION_TYPE = struct

    structure MachDepVC = PS.MachDepVC
    structure E = GenericVC.Environment
    structure PID = GenericVC.PersStamps
    structure BF = MachDepVC.Binfile
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg

    type pid = PID.persstamp

    type statenv = E.staticEnv
    type symenv = E.symenv

    type benv = statenv
    type env = { stat: statenv, sym: symenv, pids: PidSet.set }

    type 'e wpid = 'e * pid

    type envdelta = { stat: statenv wpid, sym: symenv wpid, ctxt: statenv }

    datatype lookstable_result =
	FOUND of envdelta
      | NOTFOUND of benv option

    type memorecord = { bfc: BF.bfContent, ctxt: statenv }

    structure FilterMap = BinaryMapFn
	(struct
	    type ord_key = pid * SymbolSet.set
	    fun compare ((u, f), (u', f')) =
		case PID.compare (u, u') of
		    EQUAL => SymbolSet.compare (f, f')
		  | unequal => unequal
	end)

    (* persistent state! *)
    val filtermap = ref (FilterMap.empty: pid FilterMap.map)

    fun blayer (be, be') = E.layerStatic (be, be')

    fun layer ({ stat, sym, pids }, { stat = stat', sym = sym', pids = p' }) =
	{ stat = E.layerStatic (stat, stat'),
	  sym = E.layerSymbolic (sym, sym'),
	  pids = PidSet.union (pids, p') }

    fun bfilter (d: envdelta, s) =
	E.filterStaticEnv (#1 (#stat d), SymbolSet.listItems s)

    fun pidset (p1, p2) =
	PidSet.add (PidSet.singleton p1, p2)

    fun filter (d, s) = let
	val stat = bfilter (d, s)
	val (sym, sympid) = #sym d
	val statpid = #2 (#stat d)
	val ctxt = #ctxt d
	val key = (statpid, s)
	val statpid' =
	    case FilterMap.find (!filtermap, key) of
		SOME statpid' => statpid'
	      | NONE => let
		    val statpid' = GenericVC.MakePid.makePid (ctxt, stat)
		in
		    filtermap := FilterMap.insert (!filtermap, key, statpid');
		    statpid'
		end
    in
	{ stat = stat, sym = sym, pids = pidset (statpid', sympid) }
    end

    fun bnofilter (d: envdelta) = #1 (#stat d)

    fun nofilter (d: envdelta) = let
	val (stat, statpid) = #stat d
	val (sym, sympid) = #sym d
    in
	{ stat = stat, sym = sym, pids = pidset (statpid, sympid) }
    end

    fun primitive c p = let
	val e = Primitive.env c p
	val { statpid, sympid, ctxt } = Primitive.pidInfo c p
    in
	{ stat = (E.staticPart e, statpid),
	  sym = (E.symbolicPart e, sympid),
	  ctxt = ctxt }
    end

    fun memo2envdelta { bfc, ctxt } =
	{ stat = (BF.senvOf bfc, BF.staticPidOf bfc),
	  sym = (BF.symenvOf bfc, BF.lambdaPidOf bfc),
	  ctxt = ctxt }

    fun lookstable (i, mkenv, gp) =
	case PS.recomp_look_stable i of
	    SOME memo => FOUND (memo2envdelta memo)
	  | NONE => NOTFOUND (mkenv ())

    fun dostable (i, be, gp: GeneralParams.params) = let
	val fnp = #fnpolicy gp
	val stable = FilenamePolicy.mkStablePath fnp (BinInfo.group i)
	val os = BinInfo.offset i
	val descr = BinInfo.describe i
	val _ = Say.vsay (concat ["[consulting ", descr, "]\n"])
	val s = AbsPath.openBinIn stable
	fun load () = let
	    val _ = Seek.seek (s, os)
	    val bfc = BF.read { stream = s, name = descr, senv = be,
			        keep_code = true }
	    val memo = { bfc = bfc, ctxt = be }
	in
	    BinIO.closeIn s;
	    PS.recomp_memo_stable (i, memo);
	    memo2envdelta memo
	end
    in
	SOME (load ()) handle exn => let
	    fun pphist pps =
		(PP.add_string pps (General.exnMessage exn);
		 PP.add_newline pps)
	in
	    BinIO.closeIn s;
	    BinInfo.error gp i EM.COMPLAIN
	         "unable to load stable library module" pphist;
	    NONE
	end
    end

    fun looksml (i, e: env, gp) =
	Option.map memo2envdelta (PS.recomp_look_sml (i, #pids e, gp))

    fun dosml (i, { stat, sym, pids }, gp) = let

	val mkBinPath = FilenamePolicy.mkBinPath (#fnpolicy gp)
	val binpath = mkBinPath (SmlInfo.sourcepath i)
	val binname = AbsPath.name binpath
	fun delete () = OS.FileSys.remove binname handle _ => ()

	fun save bfc = let
	    val s = AbsPath.openBinOut binpath
	    fun writer () =
		BF.write { stream = s, content = bfc, keep_code = true }
	in
	    Interrupt.guarded writer
	    handle exn => (BinIO.closeOut s; raise exn);
	    BinIO.closeOut s;
	    Say.vsay (concat ["wrote ", binname, "]\n"])
	end handle e as Interrupt.Interrupt => (delete (); raise e)
	         | exn => let
		       fun pphist pps =
			   (PP.add_string pps (General.exnMessage exn);
			    PP.add_newline pps)
		   in
		       delete ();
		       SmlInfo.error gp i EM.WARN
		                     ("failed to write " ^ binname) pphist
		   end

	fun load () = let
	    val s = AbsPath.openBinIn binpath
	    fun read () = BF.read { stream = s, name = binname, senv = stat,
				    keep_code = true }
	in
	    SOME (Interrupt.guarded read)
	    handle exn => (BinIO.closeIn s; raise exn)
	end handle e as Interrupt.Interrupt => raise e
                 | _ => NONE
    in
	Dummy.f ()
    end
end
