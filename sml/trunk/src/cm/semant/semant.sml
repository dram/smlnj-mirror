(*
 * semantic actions to go with the grammar for CM description files
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CM_SEMANT = sig

    type context = AbsPath.context
    type pathname = AbsPath.t
    type region = GenericVC.SourceMap.region
    type ml_symbol
    type cm_symbol

    type group = GroupGraph.group

    type privilegespec = GroupGraph.privilegespec
    type aexp
    type exp
    type members			(* still conditional *)
    type exports			(* still conditional *)

    type complainer = string -> unit

    (* getting elements of primitive types (pathnames and symbols) *)
    val file_native : string * context -> pathname
    val file_standard : string * context -> pathname
    val cm_symbol : string -> cm_symbol
    val ml_structure : string -> ml_symbol
    val ml_signature : string -> ml_symbol
    val ml_functor : string -> ml_symbol
    val ml_funsig : string -> ml_symbol

    (* getting the full analysis for a group/library *)
    val emptyGroup : pathname -> group
    val group :
	pathname * privilegespec * exports option * members * complainer *
	GeneralParams.info
	-> group
    val library :
	pathname * privilegespec * exports * members * complainer *
	GeneralParams.info
	-> group

    (* assembling privilege lists *)
    val initialPrivilegeSpec : privilegespec
    val require : privilegespec * cm_symbol * complainer -> privilegespec
    val grant : privilegespec * cm_symbol * complainer -> privilegespec

    (* constructing member collections *)
    val emptyMembers : members
    val member :
	GeneralParams.info * (pathname -> group)
	-> { sourcepath: pathname, group: pathname * region,
	     class: cm_symbol option }
	-> members
    val members : members * members -> members
    val guarded_members :
	exp * (members * members) * (string -> unit) -> members
    val error_member : (unit -> unit) -> members

    (* constructing export lists *)
    val emptyExports : exports
    val export : ml_symbol -> exports
    val exports : exports * exports -> exports
    val guarded_exports :
	exp * (exports * exports) * (string -> unit) -> exports
    val error_export : (unit -> unit) -> exports

    (* arithmetic (number-valued) expression *)
    val number : int -> aexp
    val variable : cm_symbol -> aexp
    val plus : aexp * aexp -> aexp
    val minus : aexp * aexp -> aexp
    val times : aexp * aexp -> aexp
    val divide : aexp * aexp -> aexp
    val modulus : aexp * aexp -> aexp
    val negate : aexp -> aexp

    (* (bool-valued) expressions *)
    val ml_defined : ml_symbol -> exp
    val cm_defined : cm_symbol -> exp
    val conj : exp * exp -> exp
    val disj : exp * exp -> exp
    val beq : exp * exp -> exp
    val bne : exp * exp -> exp
    val not : exp -> exp
    val lt : aexp * aexp -> exp
    val le : aexp * aexp -> exp
    val gt : aexp * aexp -> exp
    val ge : aexp * aexp -> exp
    val eq : aexp * aexp -> exp
    val ne : aexp * aexp -> exp
end

structure CMSemant :> CM_SEMANT = struct

    structure SymPath = GenericVC.SymPath
    structure EM = GenericVC.ErrorMsg
    structure GG = GroupGraph

    type pathname = AbsPath.t
    type context = AbsPath.context
    type region = GenericVC.SourceMap.region
    type ml_symbol = Symbol.symbol
    type cm_symbol = string

    type group = GG.group
    type privilegespec = GG.privilegespec

    type environment = MemberCollection.collection

    type aexp = environment -> int
    type exp = environment -> bool
    type members = environment -> MemberCollection.collection
    type exports = environment -> SymbolSet.set

    type complainer = string -> unit

    fun saveEval (exp, env, error) =
	exp env
	handle exn =>
	    (error ("expression raises exception: " ^ General.exnMessage exn);
	     false)

    fun file_native (s, d) = AbsPath.native { context = d, spec = s }
    fun file_standard (s, d) = AbsPath.standard { context = d, spec = s }
    fun cm_symbol s = s
    val ml_structure = Symbol.strSymbol
    val ml_signature = Symbol.sigSymbol
    val ml_functor = Symbol.fctSymbol
    val ml_funsig = Symbol.fsigSymbol

    fun applyTo mc e = e mc

    fun emptyGroup path =
	GG.GROUP { exports = SymbolMap.empty,
		   islib = false,
		   privileges = { required = StringSet.empty,
				  granted = StringSet.empty },
		   grouppath = path,
		   subgroups = [] }
	

    fun group (g, p, e, m, error, gp) = let
	val mc = applyTo MemberCollection.empty m
	val filter = Option.map (applyTo mc) e
	val (exports, rp) = MemberCollection.build (mc, filter, error, gp)
	val subgroups = MemberCollection.subgroups mc
	val { required = rp', granted = gr } = p
	val rp'' = StringSet.difference (StringSet.union (rp, rp'), gr)
	val p' = { required = rp'', granted = gr }
    in
	GG.GROUP { exports = exports, islib = false,
		   privileges = p', grouppath = g,
		   subgroups = subgroups }
    end

    fun library (g, p, e, m, error, gp) = let
	val mc = applyTo MemberCollection.empty m
	val filter = applyTo mc e
	val (exports, rp) = MemberCollection.build (mc, SOME filter, error, gp)
	val subgroups = MemberCollection.subgroups mc
	val { required = rp', granted } = p
	val p' = { required = StringSet.union (rp, rp'), granted = granted }
    in
	GG.GROUP { exports = exports, islib = true,
		   privileges = p', grouppath = g,
		   subgroups = subgroups }
    end

    local
	val isMember = StringSet.member
	fun sanity ({ required, granted }, s, error) =
	    if isMember (required, s) orelse isMember (granted, s) then
		error ("duplicate privilege name: " ^ s)
	    else ()
    in
	val initialPrivilegeSpec = { required = StringSet.empty,
				     granted = StringSet.empty }
	fun require (a as ({ required, granted }, s, _)) =
	    (sanity a;
	     { required = StringSet.add (required, s), granted = granted })
	fun grant (a as ({ required, granted }, s, _)) =
	    (sanity a;
	     { required = required, granted = StringSet.add (granted, s) })
    end

    fun emptyMembers env = env
    fun member (gp, rparse) arg env = let
	val coll = MemberCollection.expandOne (gp, rparse) arg
	val group = #group arg
	val error = GroupReg.error (#groupreg gp) group
	fun e0 s = error EM.COMPLAIN s EM.nullErrorBody
    in
	MemberCollection.sequential (env, coll, e0)
    end
    fun members (m1, m2) env = m2 (m1 env)
    fun guarded_members (c, (m1, m2), error) env =
	if saveEval (c, env, error) then m1 env else m2 env
    fun error_member thunk env = (thunk (); env)

    fun emptyExports env = SymbolSet.empty
    fun export s env = SymbolSet.singleton s
    fun exports (e1, e2) env = SymbolSet.union (e1 env, e2 env)
    fun guarded_exports (c, (e1, e2), error) env =
	if saveEval (c, env, error) then e1 env else e2 env
    fun error_export thunk env = (thunk (); SymbolSet.empty)

    fun number i _ = i
    fun variable v e = MemberCollection.num_look e v
    fun plus (e1, e2) e = e1 e + e2 e
    fun minus (e1, e2) e = e1 e - e2 e
    fun times (e1, e2) e = e1 e * e2 e
    fun divide (e1, e2) e = e1 e div e2 e
    fun modulus (e1, e2) e = e1 e mod e2 e
    fun negate ex e = ~(ex e)

    fun ml_defined s e = MemberCollection.ml_look e s
    fun cm_defined s e = MemberCollection.cm_look e s
    fun conj (e1, e2) e = e1 e andalso e2 e
    fun disj (e1, e2) e = e1 e orelse e2 e
    fun beq (e1: exp, e2) e = e1 e = e2 e
    fun bne (e1: exp, e2) e = e1 e <> e2 e
    fun not ex e = Bool.not (ex e)
    fun lt (e1, e2) e = e1 e < e2 e
    fun le (e1, e2) e = e1 e <= e2 e
    fun gt (e1, e2) e = e1 e > e2 e
    fun ge (e1, e2) e = e1 e >= e2 e
    fun eq (e1: aexp, e2) e = e1 e = e2 e
    fun ne (e1: aexp, e2) e = e1 e <> e2 e
end
