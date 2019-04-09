(*
 * iii.sml -- Implementation if `Import Identifiers'
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

functor IIDFun (structure Compiler: COMPILER): IID = struct

    structure Compiler = Compiler
    structure Env = Compiler.Environment
    structure Pid = Compiler.PersStamps
    structure Comp = Compiler.Compile
    type senv = Compiler.Environment.staticEnv
    type pid = Pid.persstamp

    type t = { senv: pid, lambda: pid }
    type set = t Set.set

    fun pid_eq (x, y) = Pid.compare (x, y) = EQUAL
    fun pid_lt (x, y) = Pid.compare (x, y) = LESS

    fun eq (i1: t, i2: t) =
	pid_eq (#senv i1, #senv i2) andalso
	pid_eq (#lambda i1, #lambda i2)
    fun lt (i1: t, i2: t) =
	pid_lt (#senv i1, #senv i2) orelse
	       pid_eq (#senv i1, #senv i2) andalso
	       pid_lt (#lambda i1, #lambda i2)

    fun new x = x

    fun staticPid { senv, lambda } = senv
    fun lambdaPid { senv, lambda } = lambda

    fun aug ({ senv, lambda }, context, senv') =
	{ senv = Comp.makePid (context, senv'), lambda = lambda }

    val { union, makeset, isSubset, ... } = Set.gen { eq = eq, lt = lt }

    fun toHex (s, r0) = let
	fun one ({ senv, lambda }, r) =
	    "[" :: Pid.toHex senv :: ":" :: Pid.toHex lambda :: "]" :: r
    in
	foldr one r0 (Set.makelist s)
    end
end
