(*
 * "Primitive" classes in CM.
 *   - provide access to compiler internals in an orderly fashion
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature PRIMITIVE = sig

    type configuration
    type primitive

    val eq : primitive * primitive -> bool

    val fromString : string -> primitive option
    val toString : primitive -> string

    val reqpriv : primitive -> StringSet.set

    (* the domain of (lookup p) must always properly include (exports p) *)
    val exports : configuration -> primitive -> SymbolSet.set
    val lookup : configuration -> primitive -> Symbol.symbol -> DAEnv.value
    val env : configuration -> primitive -> GenericVC.Environment.environment
    val pidInfo : configuration -> primitive
	-> { statpid: GenericVC.PersStamps.persstamp,
	     sympid: GenericVC.PersStamps.persstamp,
	     ctxt: GenericVC.Environment.staticEnv }

    val configuration :
	{ basis: GenericVC.Environment.environment }
	-> configuration
end

structure Primitive :> PRIMITIVE = struct

    structure BE = GenericVC.BareEnvironment
    structure E = GenericVC.Environment
    structure DE = DAEnv

    (* For now, we only know about the "basis".
     * This is for testing only -- the basis will become a real
     * "non-primitive" library, and there will be other primitives
     * that are used to implement the basis. *)
    datatype primitive =
	BASIS

    type pinfo = { exports : SymbolSet.set,
		   lookup : Symbol.symbol -> DE.value,
		   env : GenericVC.Environment.environment }

    type configuration = primitive -> pinfo

    fun eq (p1 : primitive, p2) = p1 = p2

    fun fromString "basis" = SOME BASIS
      | fromString _ = NONE

    fun toString BASIS = "basis"

    val reqpriv_basis = StringSet.empty

    fun reqpriv BASIS = reqpriv_basis

    fun exports (cfg: configuration) p = #exports (cfg p)
    fun lookup (cfg: configuration) p = #lookup (cfg p)
    fun env (cfg: configuration) p = #env (cfg p)

    fun configuration { basis } = let

	fun gen_pinfo e = let
	    fun l2s l = let
		fun addModule (sy, set) =
		    case Symbol.nameSpace sy of
			(Symbol.STRspace | Symbol.SIGspace |
			 Symbol.FCTspace | Symbol.FSIGspace) =>
			SymbolSet.add (set, sy)
		       | _ => set
	    in
		foldl addModule SymbolSet.empty l
	    end
	    
	    fun cvt_fctenv { symbols, look } =
		{ looker = cvt_result o look, domain = l2s o symbols }

	    and cvt_result (BE.CM_ENV cme) = SOME (DE.FCTENV (cvt_fctenv cme))
	      | cvt_result BE.CM_NONE = NONE

	    val sb = BE.staticPart (GenericVC.CoerceEnv.e2b e)

	    val { domain, looker } =
		cvt_fctenv { symbols = fn () => BE.catalogEnv sb,
			     look = BE.cmEnvOfModule sb }
	in
	    { exports = domain (), lookup = valOf o looker, env = e }
	end

	val basis_pinfo = gen_pinfo basis
	fun cfg BASIS = basis_pinfo
    in
	cfg
    end
    (* this doesn't make much sense yet -- there aren't any singular
     * pids describing the basis *)
    fun pidInfo c BASIS = let
	val p = GenericVC.PersStamps.fromBytes
	    (Byte.stringToBytes "0123456789abcdef")
    in
	{ statpid = p, sympid = p, ctxt = GenericVC.CMStaticEnv.empty }
    end
end
