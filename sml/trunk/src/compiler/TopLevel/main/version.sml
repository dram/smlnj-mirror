(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* version.sml *)

structure Version : sig

    val version : {
            system : string,      	(* the system title *)
	    version_id : int list,	(* the version number *)
            date : string         	(* date of creation *)
	  }

    val banner : string

  end = struct

    val version = {
	    system = "Standard ML of New Jersey ",
	    version_id = [110, 6],
	    date = "May 21, 1998"
	  }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	    #system version :: "v" :: 
	    f (#version_id version, [" [FLINT v1.41], ", #date version]))

  end


(*
 * $Log: version.sml,v $
 * Revision 1.3  1998/05/21 17:54:48  jhr
 *   Merging in Matthias's changes.
 *
 * Revision 1.2  1998/05/20 18:32:34  george
 *   Cosmetic change -- zsh
 *
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
