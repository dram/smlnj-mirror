(*
 * driver/autoload.sml: CM autoloader
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor AutoLoadFun (structure Control: CONTROL
		     structure Convert: CONVERT
		     structure Recompile: RECOMPILE
		     sharing
			 Convert.MD =
			 Recompile.SysDag.GroupDag.MD
		     and
			 Convert.Compiler =
			 Recompile.Compiler): AUTO_LOAD =
struct

    structure SD = Recompile.SysDag
    structure GD = SD.GroupDag
    structure MN = GD.ModuleName
    structure IE = GD.IE
    structure Compiler = IE.Compiler
    structure Env = Compiler.Environment
    structure BareEnv = Compiler.BareEnvironment
    structure EnvRef = Compiler.EnvRef

    exception InteractInternalError

    type envRef = EnvRef.envref
    type SCenvRef = EnvRef.SCenvref

    val known: (SD.desc * SD.analyzed_entity) list ref = ref []

    val null = MN.empty
    val combine = MN.union

    fun knownLookup mn = let
	fun loop [] = NONE
	  | loop ((_, SD.AE { namemap, ...}) :: t) =
	    case SD.lookup (mn, namemap) of
		SOME (SD.CE { dag = GD.DAG { symmap, ... }, ... }) =>
		    SOME (symmap mn)
	      | NONE => loop t
    in
	loop (!known)
    end

    fun lookup (baseLookup, localLookup) mn =
	(localLookup mn, MN.empty)
	handle IE.Undefined _ =>
	    (case knownLookup mn of
		 NONE => (baseLookup mn, MN.empty)
	       | SOME r => (r, MN.singleton mn))

    fun exclude mns = let
	fun exnm ((s, ce), rest) = let
	    val s' = MN.difference (s, mns)
	in
	    if MN.isEmpty s' then rest else (s', ce) :: rest
	end

	fun exae ((desc, ae), rest) = let
	    val SD.AE { namemap, roots, lib, stabilizer } = ae
	    val namemap' = foldl exnm [] namemap
	in
	    case namemap' of
		[] => rest
	      | _ => (desc,
		      SD.AE { namemap = namemap',
			      roots = roots, lib = lib,
			      stabilizer = stabilizer }) :: rest
	end
    in
	known := foldr exae [] (!known)
    end

    fun pick_aes imports = let
	fun p ((_, ae), (i, rest)) =
	    case SD.select_roots (ae, i) of
		{ ae = NONE, ... } => (i, rest)
	      | { ae = SOME ae', unresolved = i' } => (i', ae' :: rest)
	val (unresolved, picked) = foldl p (imports, []) (!known)
	val _ = if MN.isEmpty unresolved then ()
		else raise InteractInternalError
	fun u (SD.AE { namemap, ... }, ns) =
	    foldl (fn ((x, _), y) => MN.union (x, y)) ns namemap
	val names = foldl u MN.empty picked
    in
	(picked, names)
    end

    fun debugsyms (m, s) =
	if Control.debug NONE then
	    (Control.say (concat ["!@ autoload: ", m, "\n"]);
	     MN.fold (fn (n, ()) =>
		      Control.say (concat ["!@  ", MN.makestring n, "\n"]))
	       () s)
	else
	    ()

    fun manager (ast, baseEnvRef: SCenvRef, localEnvRef: envRef) = let
	val decl = Convert.convert { ast = ast, warn = fn _ => () }
	val baseEnv = #get baseEnvRef ()
	val baseSEnv = EnvRef.unSCstaticEnv (Env.staticPart baseEnv)
	val localEnv = #get localEnvRef ()
	val localSEnv = BareEnv.staticPart localEnv
	val baseLook = IE.mkBaseLookup baseSEnv
	val localLook = IE.mkBaseLookup localSEnv
	val look = lookup (baseLook, localLook)
	val (_, imports, exportsThunk) =
	    IE.imports (decl, null, look, combine, "<toplevel loop>")
	val exports = exportsThunk ()
	val _ = debugsyms ("demanded", imports)
	val (picked, inames) = pick_aes imports
	val _ = debugsyms ("imported", inames)
	fun run1 (ae, (delta, env)) = let
	    val delta' = Recompile.and'run'once (ae, env)
	in
	    (Env.layerEnv (delta', delta), Env.concatEnv (delta', env))
	end
	fun picksay m =
	    case picked of
		[] => ()
	      | _ => Control.vsay m
	val _ = picksay "[Autoloading...]\n"
	val (delta, _) = foldl run1 (Env.emptyEnv, baseEnv) picked
	val _ = exclude (MN.union (inames, exports))
	val localEnv' = BareEnv.concatEnv (EnvRef.unSCenv delta, localEnv)
    in
	#set localEnvRef localEnv';
	picksay "[Autoloading done.]\n"
    end

    fun register desc'n'ae =
	(Control.vsay "[Registering for AUTOLOAD...]\n";
	 known := desc'n'ae :: (!known))

    fun clear () = known := []

    fun autolist () = map #1 (!known)

end
