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
	    system = "Standard ML of New Jersey",
	    version_id = [110,0,7],
	    date = "September 28, 2000"
	  }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
	    #system version :: ", Version " ::
	    f (#version_id version, [", ", #date version]))

  end


(*
 * $Log: version.sml,v $
 * Revision 1.4  2000/10/02 14:55:19  dbm
 * new 110.0.7 release date
 *
 * Revision 1.3  2000/07/11 15:11:07  dbm
 * changed version date
 *
 * Revision 1.2  2000/05/12 12:57:15  jhr
 *   New version number (110.0.7)
 *
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.22.2.3  1999/09/08 01:06:03  jhr
 *   Updated version tag.
 *
 * Revision 1.22.2.2  1999/07/09 15:09:23  jhr
 *   New version date
 *
 * Revision 1.22.2.1  1999/06/17 18:56:40  jhr
 *   New version tag.
 *
 * Revision 1.22  1998/01/30 16:02:35  jhr
 *   New version number (110.0.3)
 *
 * Revision 1.21  1998/01/16 01:47:14  jhr
 *   New version number (110.0.2)
 *
 * Revision 1.20  1998/01/08 22:34:50  dbm
 *   Version 110.0.1
 *
 * Revision 1.19  1997/12/09  03:44:38  dbm
 *   Version 110, December 9, 1997.  Hurrah!
 *
 * Revision 1.18  1997/12/06  16:44:33  dbm
 *   Version 109.35, December 6, 1997
 *
 * Revision 1.17  1997/12/02  22:15:23  jhr
 *   New version number (109.34)
 *
 * Revision 1.16  1997/11/24  22:29:10  dbm
 *   Update version to 110.
 *
 * Revision 1.15  1997/11/21  18:34:32  dbm
 *   Update version to 109.33 (candidate 110).
 *
 * Revision 1.14  1997/10/01  14:52:58  jhr
 *   new version number (109.32)
 *
 * Revision 1.13  1997/09/08  16:33:58  jhr
 *   New version (109.31)
 *
 * Revision 1.12  1997/07/16  19:43:36  dbm
 *   109.30
 *
 * Revision 1.11  1997/06/12  14:05:19  jhr
 *   new version number
 *
 * Revision 1.10  1997/05/22  20:16:55  jhr
 * New version number (109.28)
 *
 * Revision 1.9  1997/04/18  15:46:23  george
 *   Version 109.27
 *
 * Revision 1.8  1997/04/02  13:19:34  george
 *   Version 109.26.1
 *
 * Revision 1.7  1997/03/25  13:43:18  george
 *   Version 109.26.
 *
 * Revision 1.6  1997/03/03  17:10:44  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.5  1997/02/27  02:09:55  george
 *   Version 109.25.1
 *
 * Revision 1.3  1997/02/01  11:54:52  george
 *   Version 109.25
 *
 * Revision 1.2  1997/01/31  20:39:54  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:28  george
 *   Version 109.24
 *
 *)
