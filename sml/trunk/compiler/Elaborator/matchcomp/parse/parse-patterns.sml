(* parse-patterns.sml *)

structure ParsePat =
struct

exception NoPats

(* parsepats : string * env -> Absyn.pat list *)
fun parsepats (s: string, env) =
    let val instream = StringStream.mkInstream s
    in case Parser.patterns instream
	of NONE => raise NoPats
	 | SOME(pats,instream') => 
	     map (fn p => ElaboratePat.elaborate(p,env)) pats
    end

end (* structure ParsePat *)
