(* posix-sysdb.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 system data-base operations
 *
 *)

structure POSIX_Sys_DB =
  struct

    structure FS = POSIX_FileSys

    fun cfun x = CInterface.c_function "POSIX-SysDB" x

    type word = SysWord.word
    type uid = FS.uid
    type gid = FS.gid
    
    structure Passwd =
      struct
        datatype passwd = PWD of {             (* extensible *)
             name : string,
             uid : uid,
             gid : gid,
             home : string,
             shell : string
           }

        fun name (PWD{name,...}) = name
        fun uid (PWD{uid,...}) = uid
        fun gid (PWD{gid,...}) = gid
        fun home (PWD{home,...}) = home
        fun shell (PWD{shell,...}) = shell

      end

    structure Group =
      struct
        datatype group = GROUP of {              (* extensible *)
             name : string,
             gid : gid,
             members : string list
           }

        fun name (GROUP{name,...}) = name
        fun gid (GROUP{gid,...}) = gid
        fun members (GROUP{members,...}) = members
    
      end
    
    val getgrgid' : word -> string * word * string list = cfun "getgrgid"
    val getgrnam' : string -> string * word * string list = cfun "getgrnam"
    fun getgrgid (FS.GID gid) = let
          val (name,gid,members) = getgrgid' gid
          in
            Group.GROUP { name = name,
              gid = FS.GID gid,
              members = members
            }
          end
    fun getgrnam gname = let
          val (name,gid,members) = getgrnam' gname
          in
            Group.GROUP { name = name,
              gid = FS.GID gid,
              members = members
            }
          end
    val getpwuid' : word -> string * word * word * string * string = cfun "getpwuid"
    val getpwnam' : string -> string * word * word * string * string = cfun "getpwnam"
    fun getpwuid (FS.UID uid) = let
          val (name,uid,gid,dir,shell) = getpwuid' uid
          in
            Passwd.PWD { name = name,
              uid = FS.UID uid,
              gid = FS.GID gid,
              home = dir,
              shell = shell
            }
          end
    fun getpwnam name = let
          val (name,uid,gid,dir,shell) = getpwnam' name
          in
            Passwd.PWD { name = name,
              uid = FS.UID uid,
              gid = FS.GID gid,
              home = dir,
              shell = shell
            }
          end

  end (* structure POSIX_Sys_DB *)

(*
 * $Log: posix-sysdb.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:57  george
 * Version 110.5
 *
 *)
