(*
 * Bundling information pertaining to one SML source file.
 *   - only includes information that does not require running
 *     the machine-dependent part of the compiler
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SMLINFO = sig

    type info

    type complainer = GenericVC.ErrorMsg.complainer
    type ast = GenericVC.Ast.dec
    type region = GenericVC.SourceMap.region

    val resync : unit -> unit		(* rebuild internal table *)

    val eq : info * info -> bool	(* compares sourcepaths *)
    val compare : info * info -> order	(* compares sourcepaths *)

    val info : GeneralParams.params ->
	{ sourcepath: AbsPath.t,
	  group: AbsPath.t * region,
	  share: bool option }
	-> info

    val sourcepath : info -> AbsPath.t
    val error : GeneralParams.params -> info -> complainer

    val parsetree : GeneralParams.params -> info -> ast option
    val exports : GeneralParams.params -> info  -> SymbolSet.set
    val skeleton : GeneralParams.params -> info -> Skeleton.decl
    val share : info -> bool option

    (* different ways of describing an sml file using group and source *)
    val spec : info -> string		(* sspec *)
    val fullSpec : info -> string	(* gspec(sspec) *)
    val name : info -> string		(* sname *)
    val fullName : info -> string	(* gname(sspec) *)
end

structure SmlInfo :> SMLINFO = struct

    structure Source = GenericVC.Source
    structure Print = GenericVC.Control.Print
    structure SF = GenericVC.SmlFile
    structure EM = GenericVC.ErrorMsg
    structure FNP = FilenamePolicy

    type source = Source.inputSource
    type ast = GenericVC.Ast.dec
    type region = GenericVC.SourceMap.region

    type complainer = EM.complainer

    datatype persinfo =
	PERS of { group: AbsPath.t * region,
		  lastseen: TStamp.t ref,
		  parsetree: { tree: ast, source: source } option ref,
		  skeleton: Skeleton.decl option ref }
		      
    datatype info =
	INFO of { sourcepath: AbsPath.t,
		  persinfo: persinfo,
		  share: bool option }

    fun sourcepath (INFO { sourcepath = sp, ... }) = sp
    fun share (INFO { share = s, ... }) = s

    fun gerror (gp: GeneralParams.params) = GroupReg.error (#groupreg gp)

    fun error gp (INFO { persinfo = PERS { group, ... }, ... }) =
	gerror gp group

    fun compare (INFO { sourcepath = p, ... }, INFO { sourcepath = p', ... }) =
	AbsPath.compare (p, p')
    fun eq (i, i') = compare (i, i') = EQUAL

    (* If files change their file ids, then CM will be seriously
     * disturbed because the ordering relation will change.
     * We'll asume that this won't happen in general.  However, we provide
     * a "resync" function that -- at the very least -- should be run
     * at startup time. *)
    val knownInfo = ref (AbsPathMap.empty: persinfo AbsPathMap.map)

    fun resync () = let
	val l = AbsPathMap.listItemsi (!knownInfo)
    in
	AbsPath.newEra ();		(* force recalculation of file ids *)
	knownInfo := foldl AbsPathMap.insert' AbsPathMap.empty l
    end

    fun info (gp: GeneralParams.params) arg = let
	val { sourcepath, group = gr as (group, region), share } = arg
	val groupreg = #groupreg gp
	fun newpersinfo () = let
	    val pi = PERS { group = gr, lastseen = ref TStamp.NOTSTAMP,
			    parsetree = ref NONE, skeleton = ref NONE }
	in
	    knownInfo := AbsPathMap.insert (!knownInfo, sourcepath, pi);
	    pi
	end
	fun persinfo () =
	    case AbsPathMap.find (!knownInfo, sourcepath) of
		NONE => newpersinfo ()
	      | SOME (pi as PERS { group = gr' as (g, r), ... }) =>
		    if AbsPath.compare (group, g) <> EQUAL then
			(if GroupReg.registered groupreg g then
			     let val n = AbsPath.name sourcepath
			     in gerror gp gr EM.COMPLAIN
				 (concat ["ML source file ", n,
					  " appears in more than one group"])
				 EM.nullErrorBody;
				gerror gp gr' EM.COMPLAIN
				 (concat ["(previous occurence of ", n, ")"])
				 EM.nullErrorBody
			     end
			 else ();
			 newpersinfo ())
		    else pi
    in
	INFO { sourcepath = sourcepath,
	       persinfo = persinfo (),
	       share = share }
    end

    (* check timestamp and throw away any invalid cache *)
    fun validate (INFO ir) = let
	(* don't use "..." pattern to have the compiler catch later
	 * additions to the type! *)
	val { sourcepath, persinfo = PERS pir, share } = ir
	val { group, lastseen, parsetree, skeleton } = pir
	val ts = !lastseen
	val nts = AbsPath.tstamp sourcepath
    in
	if TStamp.earlier (ts, nts) then
	    (lastseen := nts;
	     parsetree := NONE;
	     skeleton := NONE)
	else ()
    end

    (* the following functions are only concerned with getting the data,
     * not with checking time stamps *)
    fun getParseTree gp (i as INFO ir, quiet, noerrors) = let
	val { sourcepath, persinfo = PERS { parsetree, ... }, ... } = ir
	val name = AbsPath.name sourcepath
	val err = if noerrors then (fn m => ())
		  else (fn m => error gp i EM.COMPLAIN m EM.nullErrorBody)
    in
	case !parsetree of
	    SOME pt => SOME pt
	  | NONE => let
		val stream = AbsPath.openTextIn sourcepath
		val _ = if noerrors orelse quiet then ()
			else Say.vsay (concat ["[parsing ", name, "]\n"])
		val source =
		    Source.newSource (name, 1, stream, false,
				      { linewidth = !Print.linewidth,
				        flush = Print.flush,
					consumer = Print.say })
		val pto = let
		    val tree = SF.parse source
		in
		    SOME { tree = tree, source = source }
		end handle SF.Compile msg => (TextIO.closeIn stream;
					      err msg;
					      NONE)
		         | exn => (TextIO.closeIn stream; raise exn)
	    in
		TextIO.closeIn stream;
		parsetree := pto;
		pto
	    end handle exn as IO.Io _ => (err (General.exnMessage exn);
					  NONE)
    end

    fun getSkeleton gp (i as INFO ir, noerrors) = let
	val { sourcepath, persinfo = PERS pir, ... } = ir
	val { skeleton, lastseen, ... } = pir
    in
	case !skeleton of
	    SOME sk => sk
	  | NONE => let
		val skelpath = FNP.mkSkelPath (#fnpolicy gp) sourcepath
	    in
		case SkelIO.read (skelpath, !lastseen) of
		    SOME sk => (skeleton := SOME sk; sk)
		  | NONE =>
			(case getParseTree gp (i, false, noerrors) of
			     SOME { tree, source } => let
				 fun err sv region s =
				     EM.error source region sv s
					 EM.nullErrorBody
				 val { skeleton = sk, complain } =
				     SkelCvt.convert { tree = tree, err = err }
			     in
				 if noerrors then () else complain ();
				 if EM.anyErrors (EM.errors source) then
				     if noerrors then ()
				     else error gp i EM.COMPLAIN
					   "error(s) in ML source file"
					   EM.nullErrorBody
				 else (SkelIO.write (skelpath, sk);
				       skeleton := SOME sk);
				 sk
			     end
			   | NONE => Skeleton.Ref SymbolSet.empty)
	    end
    end

    (* first check the time stamp, then do your stuff... *)
    fun skeleton0 noerrors gp i = (validate i; getSkeleton gp (i, noerrors))
 
    (* we only complain at the time of getting the exports *)
    fun exports gp i = SkelExports.exports (skeleton0 false gp i)
    val skeleton = skeleton0 true

    fun parsetree gp i =
	Option.map #tree (getParseTree gp (i, true, true))

    fun spec (INFO { sourcepath, ... }) = AbsPath.spec sourcepath
    fun fullSpec (INFO { sourcepath, persinfo = PERS { group, ... }, ... }) =
	concat [AbsPath.spec (#1 group), "(", AbsPath.spec sourcepath, ")"]
    fun name (INFO { sourcepath, ... }) = AbsPath.name sourcepath
    fun fullName (INFO { sourcepath, persinfo = PERS { group, ... }, ... }) =
	concat [AbsPath.name (#1 group), "(", AbsPath.spec sourcepath, ")"]
end
