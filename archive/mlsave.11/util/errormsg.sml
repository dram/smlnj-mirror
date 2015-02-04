(* errormsg.sml *)
signature ERRORMSG =
 sig
    exception Syntax
    val anyErrors : bool ref
    val lineNum : int ref
    val fileName : string ref
    val debugging : bool ref
    val interactive : bool ref
    val say : string -> unit
    val warn : string -> unit
    val complain : string -> unit
    val condemn : string -> 'a
    val impossible : string -> 'a
    val debugmsg : string -> unit
    val flaggedmsg : bool ref * string -> unit
 end


structure ErrorMsg : ERRORMSG = struct

    exception Syntax

    val anyErrors = ref false
    val lineNum = ref 1
    val fileName = ref ""
    val debugging = System.Control.debugging
    val interactive = System.interactive

    fun say (msg: string) =
	(if !interactive then ()
	 else (print (!fileName); print ", "; print "line ";
               print (makestring(!lineNum)); print ": "; ());
         print msg;
	 print "\n";
         ())

    fun warn msg = say ("Warning: " ^ msg)

    fun complain (msg: string) =
	(say ("Error: " ^ msg); anyErrors := true)
 
    fun condemn msg = (complain msg; raise Syntax)
    fun impossible msg = condemn("Impossible error: "^msg)

    fun debugmsg (msg : string) =
	  if !debugging then (print msg; print "\n"; ())
	  else ()
    fun flaggedmsg (flg : bool ref, msg : string) =
	  if !debugging andalso !flg then (print msg; print "\n"; ())
	  else ()

end  (* structure ErrorMsg *)

