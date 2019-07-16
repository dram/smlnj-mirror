(*
 * srctypes/fname-rules.sml:
 *   Rules for how to make up names for CM-managed
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature FNAME_RULES = sig

    val declFileFor: AbsPath.t -> AbsPath.t
    val binFileFor: AbsPath.t -> AbsPath.t
    val stableFileFor: AbsPath.t -> AbsPath.t

    val errorTextFile: AbsPath.t
end
