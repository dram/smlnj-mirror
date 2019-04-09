(* os2-filesys.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* os2-filesys.sml
 *
 * This is the OS/2 specific file system interface.
 *
 * Peter Bertelsen, August 1995.
 *)

structure OS2_FileSys : OS2_FILESYS =
  struct

    val ++ = SysWord.orb
    val &  = SysWord.andb
    infix ++ &

    type word   = SysWord.word
    type s_int  = SysInt.int
    type offset = Offset.int

    fun os2Func x = CInterface.c_function "SMLNJ-OS2" x
    fun dateFunc x = CInterface.c_function "SMLNJ-Date" x

    datatype dir_hdl = DH of s_int

    datatype file_attr = F_RDONLY | F_HID | F_SYS | F_DIR | F_ARCH

    (* This is a temporary solution; should use something like osVal... *)
    fun attrVal a = (case a of
      F_RDONLY => 0w1 | F_HID => 0w2 | F_SYS => 0w4 | F_DIR => 0w16 | F_ARCH => 0w32
      (* endcase *)) : word
    val attrToWord = List.foldl (fn (a, w) => attrVal a ++ w) 0w0
    val attrToInt  = SysWord.wordToInt o attrToWord
    fun isAttrSet attrs a = let val w = SysWord.intToWord attrs
      in
        (w & attrVal a) <> 0w0
      end
    val allAttr = [F_RDONLY, F_HID, F_SYS, F_DIR, F_ARCH]
    fun attrList w = List.filter (isAttrSet w) allAttr

    val findFirstNm'   : (string * int) -> (s_int * string)
          = os2Func "findFirstNm"
    fun findFirstNm (s, al) = let val (d, e) = findFirstNm'(s, attrToInt al)
      in 
        (DH d, e) 
      end

    val findNextNm' : s_int -> string = os2Func "findNextNm"
    fun findNextNm (DH d) = findNextNm' d

    val findNmRestart' : (s_int * string * int) -> string
          = os2Func "findNmRestart"
    fun findNmRestart (DH d, s, al) = findNmRestart'(d, s, attrToInt al)

    val findClose' : s_int -> unit = os2Func "findClose"
    fun findClose (DH d) = findClose' d

    val queryCurDir : unit -> string = os2Func "queryCurDir"
    val setCurDir   : string -> unit = os2Func "setCurDir"
    val createDir   : string -> unit = os2Func "createDir"
    val deleteDir   : string -> unit = os2Func "deleteDir"

    structure PINFO =
      struct

        (* the tuple type used to communicate with C function queryPathInfo *)
        type path_time = int * int * int * int * int * int
                      (* year  month day   hour  min   sec *)

        datatype path_info = PINFO of {
            crtd  : path_time,
            wrttn : path_time,
            accsd : path_time,
            attrs : int,
            size  : offset
          }

        (* the tuple type used to communicate with Date library functions *)
        type tm = int * int * int * int * int * int * int * int * int
               (* sec   min   hour  day   mon   year  wday  yday  dst *)

        val mkTime'    : tm -> (int * int) = dateFunc "mkTime"
        val localTime' : (int * int) -> tm = dateFunc "localTime"
        val tmYear0   = 1900   (* base year used in tm tuple *)
        val pathYear0 = 1980   (* base year used in path_time tuple *)

        fun toTime (pt_year, pt_month, pt_day, pt_hour, pt_min, pt_sec) = let
          val (sec, usec) = mkTime' (pt_sec, pt_min, pt_hour, 
                 pt_day, pt_month-1, pt_year+pathYear0-tmYear0, 0, 0, 0)
          in
            PreBasis.TIME{sec=sec, usec=usec}
          end

        fun created  (PINFO{crtd,...})  = toTime crtd
        fun written  (PINFO{wrttn,...}) = toTime wrttn
        fun accessed (PINFO{accsd,...}) = toTime accsd
        fun attribs  (PINFO{attrs,...}) = attrList attrs
        fun hasAttr  (PINFO{attrs,...}) = isAttrSet attrs
        fun size     (PINFO{size,...})  = size

        fun fromTime (PreBasis.TIME{sec, usec}) = let
          val (tm_sec, tm_min, tm_hour, tm_day, tm_month, tm_year, _, _, _)
                = localTime' (sec, usec)
          val pt_year = tm_year + tmYear0 - pathYear0
          in
            if pt_year >= 0
            then (pt_year, tm_month+1, tm_day, tm_hour, tm_min, tm_sec)
            else raise Fail "path info cannot specify a year before 1980"
          end

        fun fromTimeOpt  NONE    = NONE
          | fromTimeOpt (SOME t) = SOME(fromTime t)

      end (* structure PINFO *)

    val queryPathInfo' : string ->
      (PINFO.path_time * PINFO.path_time * PINFO.path_time * int * offset) =
         os2Func "queryPathInfo"
    fun queryPathInfo path = let 
      val (cr, wr, acc, attrs, size) = queryPathInfo' path
      in 
        PINFO.PINFO{crtd=cr, wrttn=wr, accsd=acc, attrs=attrs, size=size}
      end
 
    val queryPathAttr' : string -> int = os2Func "queryPathAttr"
    fun queryPathAttr path = isAttrSet (queryPathAttr' path)

    val queryPathSize : string -> offset = os2Func "queryPathSize"
    val queryFullPath : string -> string = os2Func "queryFullPath"
    val checkExists   : string -> unit   = os2Func "checkExists"

    val setPathAttr' : (string * int) -> unit = os2Func "setPathAttr"
    fun setPathAttr (path, attrs) = setPathAttr'(path, attrToInt attrs)

    val setPathTime' : (string * PINFO.path_time option * 
          PINFO.path_time option * PINFO.path_time option) -> unit
            = os2Func "setPathTime"
    fun setPathTime (path, {cr, wr, acc}) = setPathTime'(path, 
          PINFO.fromTimeOpt cr, PINFO.fromTimeOpt wr, PINFO.fromTimeOpt acc)
 
    val delete : string -> unit = os2Func "delete"

    val move' : string * string -> unit = os2Func "move"
    fun move {old, new} = move' (old, new)

  end (* structure OS2_FileSys *)


(*
 * $Log: os2-filesys.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:21  george
 *   Version 109.24
 *
 *)
