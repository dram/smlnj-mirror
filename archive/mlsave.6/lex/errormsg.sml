(* errormsg.sml *)

structure ErrorMsg = struct

    exceptionx Syntax;

    val anyErrors = ref false
    val lineNum = ref 1
    val fileName = ref ""

    fun say (msg: string) =
	(case !fileName of "" => () | s => (print (s ^ ", "); ());
         print "line "; print (makestring(!lineNum)); print ": ";
         print msg; print "\n";
         ())

    fun warn msg = say ("Warning: " ^ msg)

    fun complain (msg: string) =
	(say msg; anyErrors := true)
 
    fun condemn msg = (complain msg; raisex Syntax)
    fun impossible msg = condemn("Impossible error: "^msg)

end;  (* ErrorMsg *)

