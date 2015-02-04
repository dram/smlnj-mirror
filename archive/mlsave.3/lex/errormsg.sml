(* errormsg.sml *)

structure ErrorMsg = struct

    exceptionx Syntax;

    val AnyErrors = ref false
    val LineNum = ref 1
    val FileName = ref ""

    fun Say (msg: string) =
	(case !FileName of "" => () | s => (print (s ^ ", "); ());
         print "line "; print (makestring(!LineNum)); print ": ";
         print msg; print "\n";
         ())

    fun Warn msg = Say ("Warning: " ^ msg)

    fun Complain (msg: string) =
	(Say msg; AnyErrors := true)
 
    fun Condemn msg = (Complain msg; raisex Syntax)
    fun Impossible msg = Condemn("Impossible error: "^msg)

end;  (* ErrorMsg *)

