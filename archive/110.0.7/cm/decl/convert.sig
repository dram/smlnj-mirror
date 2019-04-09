(*
 * decl/convert.sig: Converting ASTs to CM's trimmed version thereof.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature CONVERT = sig

    structure MD: MODDECL
    structure Compiler: COMPILER

    val convert: { ast: Compiler.Ast.dec, warn: int * int -> unit }  -> MD.decl

end
