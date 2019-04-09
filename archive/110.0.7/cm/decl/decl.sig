(*
 * decl/decl.sig: create and cache `MD.decl's
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature DECL = sig

    structure MD: MODDECL
    structure Compiler: COMPILER

    exception FormatError and InternalError

    (*
     * recover: declfile * sourcetime option
     *  -> MD.decl option
     *   - recover a decl data structure from the declfile specified
     *     (if possible) unless the source is newer.
     *   - if everything is ok then return SOME decl, otherwise NONE.
     *)
    val recover: AbsPath.t * TStamp.t -> MD.decl option

    (*
     * create: ast * declfile -> MD.decl
     *  - create a decl data structure from an abstract syntax tree
     *  - cache the result in declfile
     *)
    val create:	Compiler.Ast.dec * AbsPath.t * Compiler.Source.inputSource
	-> MD.decl

    datatype member =
	ENTITY of { name: AbsPath.t, class: string }
      | FILE of { name: AbsPath.t, decl: MD.decl }

    (*
     * recover_stable: stablef * cmf time * canon
     *   -> member list option
     *  read a list of sourcename * MD.decl pairs from stablefile (if this
     *  isn't older than the corresponding CM-file).
     *  Return SOME (member list) or NONE.
     *  canon: { name: string, rigid: bool } -> AbsPath.t
     *)
    val recover_stable:	AbsPath.t * Time.time *
	({ name: string, rigid: bool } -> AbsPath.t)
	-> member list option

    (*
     * create_stable:
     *  (filename * decl) list * stablefile -> unit
     *  Create stablefile.
     *)
    val create_stable: member list * AbsPath.t -> unit

end
