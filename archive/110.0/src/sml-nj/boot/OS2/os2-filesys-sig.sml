(* os2-filesys-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-filesys-sig.sml
 *
 * Signature for the OS/2 specific file system interface.
 *
 * Peter Bertelsen, August 1995.
 *)

signature OS2_FILESYS =
  sig

    type offset
      sharing type offset = Offset.int

    type dir_hdl

    datatype file_attr = F_RDONLY | F_HID | F_SYS | F_DIR | F_ARCH
    (* NOTE: 
     * Even though directories aren't really considered `true files' in
     * OS/2, every entry in a directory actually has a "sub-directory" 
     * attribute...
     *)

    val findFirstNm   : string * file_attr list -> dir_hdl * string
    val findNmRestart : dir_hdl * string * file_attr list -> string
    val findNextNm    : dir_hdl -> string
    val findClose     : dir_hdl -> unit

   (* NOTE: 
    * The DosFindFirst/Next API actually provides full path info about
    * every entry found; findFirstNm, findNextNm etc. just only return entry
    * names. But full-fledged findFirst, findNext etc. should also be 
    * implemented sometime, e.g. something like:
    *
    *  val findFirst   : string * file_attr list -> dir_hdl * string * PINFO.PINFO
    *  val findNext    : dir_hdl -> string * PINFO.PINFO
    *  val findRestart : dir_hdl * string * file_attr list -> string * PINFO.PINFO
    *)

    val queryCurDir : unit -> string
    val setCurDir   : string -> unit
    val createDir   : string -> unit
    val deleteDir   : string -> unit

    structure PINFO :
      sig
        type path_info

        val created  : path_info -> Time.time
        val written  : path_info -> Time.time
        val accessed : path_info -> Time.time
        val attribs  : path_info -> file_attr list
        val hasAttr  : path_info -> file_attr -> bool
        val size     : path_info -> offset
      end

    val queryPathInfo : string -> PINFO.path_info
    val queryPathAttr : string -> file_attr -> bool
    val queryPathSize : string -> offset

    val queryFullPath : string -> string
     (* Doesn't really check if the path exists... *)

    val checkExists   : string -> unit
     (* Raises SysErr if path doesn't exist *)

    val setPathAttr : (string * file_attr list) -> unit
    val setPathTime : (string *
      {cr: Time.time option, wr: Time.time option, acc: Time.time option}) -> unit

    val delete : string -> unit
    val move   : {old: string, new: string} -> unit

  end (* signature OS2_FILESYS *)


(*
 * $Log: os2-filesys-sig.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
