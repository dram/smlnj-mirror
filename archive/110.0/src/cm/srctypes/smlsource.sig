(*
 * srctypes/smlsource.sig:
 *   Data structure which associates SML source files with everything
 *   you ever wanted to know about them.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SML_SOURCE = sig

    structure Compiler: COMPILER
    structure MD: MODDECL
    structure CUnit: CUNIT
    structure Iid: IID

    sharing Iid = CUnit.Iid
    sharing Compiler = CUnit.Compiler

    type senv = Compiler.Environment.staticEnv
    type symenv = Compiler.Environment.symenv
    type env = Compiler.Environment.environment

    exception SourceFileDoesNotExist of string
    and UserCodeExn of string * exn
    and SmlSourceInternalError

    datatype cu =
	SUCCESS of CUnit.t
      | FAILURE of exn

    type t

    val eq: t * t -> bool

    val get: { file: AbsPath.t, creator: string option } -> t

    (*
     * mkstable: filename * MD.decl -> t
     *  Create and cache a `stable' source.  Such a source is never read.
     *  Only the corresponding binfile must exist.  There will be no
     *  stat(2)ing ever wrt. this source.
     *)
    val mkstable: AbsPath.t * MD.decl -> unit

    val name: t -> AbsPath.t		(* the file name *)
    val makestring: t -> string		(* a descriptive string *)
    val binfile: t -> AbsPath.t

    (*
     * Extract the declarations (the information used by the dependency
     * analyzer) from the source.  This might trigger parsing the file.
     *)
    val decl: t -> MD.decl

    (*
     * Produce a `compiled unit' from an SML-source.  This might just fetch
     * it from an in-core cache, it might read a binfile (and rehydrate it)
     * or it could even invoke the compiler on the source file.
     * `mkcompenv' will be run to produce the set of pids associated
     * with the context and a corresponding environmemt.
     *
     * The code is supposed to run if `runEnvOf' is `SOME f'.
     *
     * This function (as well as `cunit_again' below) returns a `future'
     * (see module `Futures') on either `SUCCESS cunit' or `FAILURE exn'.
     * In the latter case the unit couldn't be created for some reason.
     * In multiprocessor versions of CM `cunit' returns immediately
     * after spawning a new thread.
     *)
    val cunit: {
		smlsource: t,
		mkcompenv: unit -> (unit -> 'a) * Iid.set,
		senvOf: 'a -> senv,
		symenvOf: 'a -> symenv,
		runEnvOf: ('a -> env) option
	       }
	-> cu Futures.future

    (*
     * Fetch the existing cunit option future.  (`cunit' MUST have
     * been called before.)
     *)
    val cunit_again: t -> cu Futures.future

    (*
     * `get' caches its results in an internal table, `sweepcache' can
     * be used to remove cache entries, which are no longer up-to-date.
     * `clearcache' empties the entire cache.
     *)
    val clearcache: unit -> unit
    val sweepcache: unit -> unit

end
