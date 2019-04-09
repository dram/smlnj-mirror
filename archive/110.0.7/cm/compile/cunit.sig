(*
 * compile/cunit.sig: CM's interface to `compiled units'
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CUNIT = sig

    structure Compiler: COMPILER
    structure Iid: IID

    sharing Iid.Compiler = Compiler

    exception Compile of string
    exception Outdated
    exception FormatError
    exception NoCodeBug

    type senv = Compiler.Environment.staticEnv
    type denv = Compiler.Environment.dynenv
    type symenv = Compiler.Environment.symenv
    type env sharing type env = Compiler.Environment.environment

    type t

    (* `interface id' type *)
    type iid = Iid.t
    type iidset = Iid.set

    val iid: t -> iid
    val senv: t -> senv
    val symenv: t -> symenv
    val env: t -> env option

    (*
     * recover:
     *  - recover a compiled unit from the binfile specified (if
     *    possible) unless the source was newer or the unit
     *    is incompatible with the iids provided.
     *  - if everything is ok then return SOME (unit, bintime), otherwise NONE.
     *)
    val recover: {
		  srcfile: AbsPath.t,	(* for comment string *)
		  binfile: AbsPath.t,
		  se: senv,
		  sourcetime: TStamp.t,
		  provided: iidset,
		  keep_code: bool
		 }
	->
	{ u: t, bintime: TStamp.t } option

    (*
     * The core environment used by `create'.  This will be initialized to
     * the `real' core environment, but can be re-assigned to during
     * `Batch' compilation.
     *)
    val coreEnvRef: { get: unit -> Compiler.EnvRef.staticEnv,
		      set: Compiler.EnvRef.staticEnv -> unit }

    (*
     * create: ast * source * binfile * senv * iids provided -> t
     *  - create a compiled unit from an abtract syntax tree and
     *    a static environment by compiling the source file
     *  - cache the unit as a binary file in the file system
     *)
    val create: {
		 ast: Compiler.Ast.dec,
		 source: Compiler.Source.inputSource,
		 name: AbsPath.t,
		 binfile: AbsPath.t,
		 senv: senv,
		 symenv: symenv,
		 provided: iidset,
		 keep_code: bool
		}
	-> t

    (*
     * isValid: t * iids provided * need_code -> bool
     *  - determines validity for an in-core unit
     *)
    val isValid: t * iidset * bool -> bool

    (*
     * parse: sourcefile * description -> ast * source
     *  - parse sourcefile
     *  - return abstract syntax tree and a `source' suitable for
     *    subsequent compile phases (the input stream of the source is
     *    already closed)
     *)
    val parse:
	{ file: AbsPath.t, desc: string }
	->
	{ ast: Compiler.Ast.dec, source: Compiler.Source.inputSource }

    (*
     * run the core of a unit in some (dynamic) environment, return the
     * change (delta) to that environment
     *)
    val execute: t * denv -> env

    (**************************************
     * Support for `Batch' compilation... *
     **************************************)

    (*
     * compileBootFile:
     *   runtimePid option * splitting flag ->
     *   sourcefile * binfile * static env * symbolic env ->
     *   static env * symbolic env
     *)
    val compileBootFile:
	string option * bool
	-> AbsPath.t * AbsPath.t * senv * symenv
	-> senv * symenv


    (*
     * fetchUnit: binfile * static env -> t
     *)
    val fetchUnit: AbsPath.t * senv -> t

    (*
     * fetchObjectEnv: binfile * static env -> static env * symbolic env
     *  - fetch static and symbolic environments from existing binfile
     *)
    val fetchObjectEnv: AbsPath.t * senv -> senv * symenv

end
