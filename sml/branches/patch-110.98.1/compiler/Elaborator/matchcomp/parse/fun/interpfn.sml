(* interpfn.sml *)

(* interactive interpreter for Fun 
 * takes input a parameter character stream (S: CHAR_STREAM), tokenizes, parses,
 * type checks, and evaluates, then prints the resulting types and values.
 * Handles top-level "statements" (Syntax.stmt), which can be either expressions
 * or value declararions (val, fun).
 *)

functor InterpFn(S: CHAR_STREAM) : 
    sig 
      val run: S.instream -> unit
    end =
struct

  (* these are the "imported" modules *)
  structure Ty = Type
  structure V = Eval
  structure Pr = Print
  structure P = ParserFn(S)

  type instream = S.instream

  (* repl : value Env.env * ty Env.env -> unit
   * The calculator's read-eval-print loop.  It parses one input line at
   * a time. *)
  fun repl (instream, env, tyenv) =
      (* parse token stream as a statement (expr. or decl.) *)
      (case (P.top instream)
	of NONE => print ("Error - parse failure\n")
	 | SOME(stmt,instream') =>
	   (* parsed one statement, evaluate it *)
	   (let val (ty, tyenv') = Ty.typeStmt(stmt, tyenv)
            in case ty
                of Ty.ERRORty => repl(instream',env,tyenv)
                 | _ => let val (res, env') = V.evalStmt(stmt, env)
			in Pr.printStmt(ty,res); 
			   TextIO.flushOut TextIO.stdOut;
			   repl(instream', env',tyenv')
			end
	    end
	    handle Env.Unbound s => (* handling unbound variable errors *)
		   (print ("Error - unbound var: "^s^"\n");
		    repl(instream', env, tyenv))))

  (* run: R.instream -> unit
   * The top-level interpreter entry point. *)
  fun run (instream: S.instream) = 
      repl (instream, V.venvInit, Ty.tyenvInit)
      handle V.Exit => (print "exiting interpeter\n")

end (* structure Interp *)

(* Notes

*)
