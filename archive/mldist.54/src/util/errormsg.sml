(* Copyright 1989 by AT&T Bell Laboratories *)
structure ErrorMsg : ERRORMSG = struct

    exception Syntax
    exception Cascade of string

    val anyErrors = ref false
    datatype severity = WARN | COMPLAIN | CONDEMN | CASCADE | BUG
    type complainer = severity -> string -> unit

    fun say (msg: string) = (print msg; print "\n")

    fun warn msg = say ("Warning: " ^ msg)

    fun complain (msg: string) =
	(say ("Error: " ^ msg); anyErrors := true)
 
    fun impossible msg = (complain("Compiler bug: " ^ msg); raise Syntax)

    type pos = int
    type pos2 = pos * pos
    type complainer  = severity -> string -> unit 
    type inputSource  = {fileName: string,  linePos: int list ref,
	  		    lineNum: int ref, anyErrors: bool ref,
			    errStream: outstream, interactive: bool,
			    sourceStream: instream}

    fun newSource(fileName,sourceStream,interactive,errStream) =
	{fileName=fileName,sourceStream=sourceStream,interactive=interactive,
	 errStream=errStream,linePos=ref[0],lineNum=ref 1, anyErrors=ref false}

    fun filepos({fileName,linePos,lineNum,...}:inputSource) p =
	let fun look(p:int,a::rest,n) = 
		if a<p then (fileName,n,p-a) else look(p,rest,n-1)
         in look(p,!linePos,!lineNum)
        end

    fun error ({fileName,linePos,lineNum,anyErrors,
		  errStream,...}:inputSource) (p1,p2) severity s = 
	let fun look(p:int,a::rest,n) = 
		if a<p then (n,p-a) else look(p,rest,n-1)
	      | look _ = (0,0)
            val (p1line,p1pos) = look(p1,!linePos,!lineNum)
            val (p2line,p2pos) = look(p2-1,!linePos,!lineNum)
	    val kind = case severity
			of WARN => " Warning: "
			 | COMPLAIN => " Error: "
			 | CONDEMN => " Fatal Error: "
			 | CASCADE => "Compiler Bug: "
			 | BUG => " Compiler Bug: "
	    val p2stuff = if p1+1>=p2 then [] 
			  else ["-", makestring p2line, ".", makestring p2pos]
	 in if !anyErrors andalso severity=CASCADE then ()
	    else output errStream (implode(
			[fileName,":",makestring p1line,".",makestring p1pos]
			@ p2stuff @ [kind,s,"\n"]));
	    case severity of BUG => raise Syntax
		       | CONDEMN => raise Syntax
		       | CASCADE => raise Syntax
		       | COMPLAIN => anyErrors := true
		       | _ => ()	    
	end
end  (* structure ErrorMsg *)

