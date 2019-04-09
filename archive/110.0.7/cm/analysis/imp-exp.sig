(*
 * analysis/imp-exp.sig:
 *   Compute the imports and exports for one SML source file.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature IMP_EXP = sig

    structure ModuleName: MODULE_NAME
    structure MD: MODDECL
    structure Compiler: COMPILER
    sharing ModuleName = MD.ModuleName
    sharing Compiler = ModuleName.Compiler

    (* env is an opaque type carrying information about symbols *)
    type env

    val mkBaseLookup:
	Compiler.BareEnvironment.staticEnv -> ModuleName.t -> env

    val imports:
	MD.decl *
	'info *				(* null info *)
	(ModuleName.t -> env * 'info) *	(* get env and info for global sym. *)
	('info * 'info -> 'info) *	(* combine information *)
	string
	->
	(ModuleName.t -> env) * 'info * (unit -> ModuleName.set)

    (* The third part of the result from `imports' is a thunk for
     * calculating the exports of the compilation unit.  The algorithm
     * for this doesn't suffer from the restriction of not allowing
     * top-level open, so it suits the needs of the autoloader.
     * However, for dependency analysis it is useless, because we
     * need to know exports in advance.  In this case one has to use
     * the following separate function (which does not allow
     * top-level open): *)
    val exports: MD.decl -> ModuleName.set

    exception Undefined of ModuleName.t
    and IllegalToplevelOpen
    and InternalError of string

end
