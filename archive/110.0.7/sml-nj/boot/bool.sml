(* bool.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure Bool : BOOL =
  struct

    datatype bool = datatype PrimTypes.bool

    val not = InlineT.inlnot

  (* NOTE: this should probably accept a wider range of arguments, but the
   * spec hasn't been written yet.
   *)
    fun scan (getc : (char, 'a) StringCvt.reader) cs = (
	  case (getc (PreBasis.skipWS getc cs))
	   of (SOME(#"t", cs')) => (case (PreBasis.getNChars getc (cs', 3))
		 of (SOME([#"r", #"u", #"e"], cs'')) => SOME(true, cs'')
		  | _ => NONE
		(* end case *))
	    | (SOME(#"f", cs')) => (case (PreBasis.getNChars getc (cs', 4))
		 of (SOME([#"a", #"l", #"s", #"e"], cs'')) => SOME(false, cs'')
		  | _ => NONE
		(* end case *))
	    | _ => NONE
	  (* end case *))

    fun toString true = "true"
      | toString false = "false"
    val fromString = PreBasis.scanString scan

  end


(*
 * $Log: bool.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:37  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/06/02 19:14:58  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
