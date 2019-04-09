(* os-filesys-os2.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the OS/2 implementation of the generic OS.FileSys structure.
 *
 * Peter Bertelsen, August 1995.
 *
 *)

structure OS_FileSys : OS_FILE_SYS =
  struct

    structure OS2F = OS2.FileSys
    structure P = OS_Path

    datatype dirstream = DS of { 
      hdl:    OS2F.dir_hdl,   (* directory search handle *)
      srch:   string,         (* pattern to search for *)
      prev:   string ref,     (* name of previous entry *)
      isOpen: bool ref
    }

    (* NOTE: 
     *   The DosFindFirst/Next API differs from un*x opendir/readdir, in that
     *   DosFindFirst() actually returns the first matching entry. Thus, the
     *   purpose of the prev field in the dirstream type is to `delay' the
     *   returning of each matching entry until the next time readDir is 
     *   called. The search pattern is stored for later (re)use in rewindDir.
     *)
 
    (* wildcards: substring -> bool
     * Check if the substring contains any `wildcards' (i.e. * or ?)
     *)
    fun wildcards' (_, true) = true
      | wildcards' (#"*", _) = true
      | wildcards' (#"?", _) = true
      | wildcards' (_,false) = false
    val wildcards = Substring.foldl wildcards' false

    (* mkSearchStr: string -> string
     * Build a search string from a directory specification 
     *)
    fun mkSearchStr path = if wildcards (Substring.all path)
          then raise Fail "no wildcards allowed in directory name"
          else P.joinDirFile {dir=path, file="*"}
            (* the search string must end in "\*"... *)

    fun openDir path = let
      val search = mkSearchStr path
      val mustHaveAttr = []
      val (hdl, first) = OS2F.findFirstNm(search, mustHaveAttr)
      in 
        DS{hdl=hdl, srch=search, prev=ref first, isOpen = ref true}
      end

    fun readDir (DS{isOpen = ref false, ...}) =
          PreBasis.error "readDir on closed directory stream"
      | readDir (DS{hdl, prev, ...}) = let
          val entry = !prev
          in
            if (entry = "") then () else (prev:= OS2F.findNextNm hdl);
            entry
          end

    fun rewindDir (DS{isOpen = ref false, ...}) =
          PreBasis.error "rewindDir on closed directory stream"
      | rewindDir (DS{hdl, srch, prev, ...}) = let
          val mustHaveAttr = []
          in
            prev:= OS2F.findNmRestart(hdl, srch, mustHaveAttr)
          end

    fun closeDir (DS{isOpen = ref false, ...}) = ()
      | closeDir (DS{hdl, isOpen, ...}) = 
          (isOpen:= false; OS2F.findClose hdl)

    val chDir  = OS2F.setCurDir
    val getDir = OS2F.queryCurDir
    val mkDir  = OS2F.createDir
    val rmDir  = OS2F.deleteDir
    fun isDir path = OS2F.queryPathAttr path OS2F.F_DIR

    fun isLink path = (OS2F.checkExists path; false)
    fun readLink path = (OS2F.checkExists path; path)

    (* strUpper : string -> string *)
    val strUpper = String.translate (String.str o Char.toUpper)

    fun fullPath path = (OS2F.checkExists path; 
                         strUpper (OS2F.queryFullPath path))

    fun realPath p = if (P.isAbsolute p)
          then fullPath p
          else P.mkRelative(fullPath p, fullPath(getDir()))

    val modTime = OS2F.PINFO.written o OS2F.queryPathInfo

    fun setTime (path, NONE) = let val t = SOME (Time.now())
          in
            OS2F.setPathTime(path, {cr=NONE, wr=t, acc=t})
          end
      | setTime (path, t) = OS2F.setPathTime(path, {cr=NONE, wr=t, acc=t})

    val remove = OS2F.delete
    val rename = OS2F.move

    datatype access = A_READ | A_WRITE | A_EXEC
 
    (* NOTE:
     *   A_READ access is always granted in OS/2; so we just have to ensure 
     *   that the path actually exists.
     *   A_WRITE is granted if F_RDONLY isn't set (and the path exists).
     *   A_EXEC is really a un*x'ism; here we interpret it as the path being
     *   a directory or a file that may be `executed by the shell', that is,
     *   a file with extension .exe, .com or .cmd (a `script').
     *)

    fun access (path, alist) = let
      val hasAttr = OS2F.queryPathAttr path
      fun access' A_READ  = true  (* Well, it must exist since we
                                    `survived' the queryPathAttr... *)
        | access' A_WRITE = Bool.not (hasAttr OS2F.F_RDONLY)
        | access' A_EXEC  = (hasAttr OS2F.F_DIR) orelse
            (case #ext(OS_Path.splitBaseExt path) of
               SOME ext => (case (strUpper ext) of
                             ("EXE" | "COM" | "CMD") => true
                            | _ => false
                            (* end case *))
             | _ => false
             (* end case *))
      in
        List.all access' alist
      end

    fun tmpName {dir : string option, prefix : string option} =
      raise Fail "OS.FileSys.tmpName not implemented yet"

  end; (* structure OS_FileSys *)


(*
 * $Log: os-filesys-os2.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
