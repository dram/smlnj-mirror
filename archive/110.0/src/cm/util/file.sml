(*
 * util/file.sml: basic file system operations
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure File:> FILE = struct

    fun modTime n = OS.FileSys.modTime n handle _ => Time.zeroTime

    fun exists n = OS.FileSys.access (n, []) handle _ => false

end
