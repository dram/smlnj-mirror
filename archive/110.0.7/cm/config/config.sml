(*
 * config/config.sml:
 *  Default config.sml (when not using the `build' script)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure CmConfig = struct
    val lex = "ml-lex"
    val yacc = "ml-yacc"
    val burg = "ml-burg"
    val namelength_limited = false
    val path: string list = []
    val rcsco = "co -q"
end
