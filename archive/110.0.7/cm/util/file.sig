(*
 * util/file.sig: basic file system operations
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature FILE = sig

    val modTime: string -> Time.time
    val exists: string -> bool

end
