(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sml *)
 
signature EVALLOOP =
sig
  exception Interrupt 

  type obj = Unsafe.Object.object
  type obvec = obj Vector.vector
  type interactParams = 
         {compManagerHook : (Ast.dec * EnvRef.SCenvref 
                                     * EnvRef.envref -> unit) option ref,
          baseEnvRef      : EnvRef.SCenvref,
          localEnvRef     : EnvRef.envref,
          transform       : Absyn.dec -> Absyn.dec,
          instrument      : {source: Source.inputSource,
                             compenv: StaticEnv.staticEnv}
                                -> Absyn.dec -> Absyn.dec,
          perform         : (obvec -> obj) -> (obvec -> obj),
          isolate         : (obvec -> obj) -> (obvec -> obj),
          printer         : Environment.environment -> PrettyPrint.ppstream 
                                -> (Absyn.dec * Lambda.lvar list) -> unit}

  val stdParams   : interactParams
  val interact    : interactParams -> unit
  val evalStream : interactParams -> string * TextIO.instream -> unit

end (* signature EVALLOOP *)

functor EvalLoopF(Compile: TOP_COMPILE) : EVALLOOP =
struct

local open Environment
      structure C  = Compile
      structure EM = ErrorMsg
      structure E  = Environment
      structure EU = ElabUtil
      structure SCS = SCStaticEnv      
      structure T = Time
      structure U = Unsafe
in 

exception Interrupt
type obj = Unsafe.Object.object
type obvec = obj Vector.vector

type interactParams =
       {compManagerHook : (Ast.dec * EnvRef.SCenvref 
                                   * EnvRef.envref -> unit) option ref,
        baseEnvRef      : EnvRef.SCenvref,
        localEnvRef     : EnvRef.envref,
        transform       : Absyn.dec -> Absyn.dec,
        instrument      : {source: Source.inputSource, 
                           compenv: staticEnv}
                             -> Absyn.dec -> Absyn.dec,
        perform         : (obvec -> obj) -> (obvec -> obj),
        isolate         : (obvec -> obj) -> (obvec -> obj), 
        printer         : E.environment -> PrettyPrint.ppstream 
                             -> Absyn.dec * Lambda.lvar list -> unit}
        

val stdParams : interactParams =
      {compManagerHook = ref NONE,
       baseEnvRef = EnvRef.pervasive,
       localEnvRef = EnvRef.topLevel,
       transform = (fn x => x),
       instrument = (fn _ => fn x => x),
       perform = (fn x => x),
       isolate = Compile.isolate,
       printer = PPDec.ppDec}

(* toplevel loop *)

val say = Control.Print.say
fun debugmsg msg = 
  if !Control.debugging	then (say (msg ^ "\n"); Control.Print.flush()) else ()

exception EndOfFile

fun codegen(arg)=
    let val code = C.codegen arg
	val _ = debugmsg "about to boot\n"
     in C.applyCode code 
    end

fun interruptable f x =
    let val oldcont = !U.topLevelCont
     in U.topLevelCont :=
         SMLofNJ.Cont.callcc
            (fn k => (SMLofNJ.Cont.callcc(fn k' => (SMLofNJ.Cont.throw k k'));
	              raise Interrupt));
        (f x before U.topLevelCont := oldcont)
        handle e => (U.topLevelCont := oldcont; raise e)
    end

(* 
 * The baseEnv and localEnv are purposely refs so that a top-level command
 * can re-assign either one of them, and the next iteration of the loop
 * will see the new value. It's also important that the toplevelenv
 * continuation NOT see the "fetched" environment, but only the ref;
 * then, if the user "filters" the environment ref, a smaller image
 * can be written. 
 *)

fun evalLoop ({compManagerHook, baseEnvRef, localEnvRef, perform, 
               isolate, printer, instrument, transform} : interactParams)
             (source: Source.inputSource) : unit =

let val parser = C.parseOne source
    val compInfo = C.mkCompInfo(source,#get EnvRef.core (),transform)
    val baseEnvRefunSC = EnvRef.unSC baseEnvRef

    fun checkErrors () = 
        if C.anyErrors compInfo then raise EM.Error else ()

    (*** !!! the environment conversions need a clean-up (ZHONG) !!! ***)
    fun oneUnit () = (* perform one transaction  *)
	case parser ()
	  of NONE => raise EndOfFile
	   | SOME ast =>
	     let val _ = case !compManagerHook
			   of NONE => ()
			    | SOME cm => cm (ast, baseEnvRef, localEnvRef)

		 val {static=statenv,dynamic=dynenv, symbolic=symenv} =
		     E.layerEnv(#get localEnvRef (), #get baseEnvRefunSC ())

		 val {absyn,newenv,exportLvars,exportPid,...} =
		       C.elaborate{ast=ast,compenv=statenv,compInfo=compInfo}
		       before checkErrors ()
		       handle C.Compile _ => raise EM.Error

		 val absyn = 
		       C.instrument
		         {compenv=statenv,source=source,compInfo=compInfo}
			 (instrument {compenv=statenv,source=source} absyn)

		 val {genLambda,imports=importPids} = 
		       C.translate{absyn=absyn, exportLvars=exportLvars,
				   exportPid=NONE,
				   newstatenv=newenv,
				   oldstatenv=statenv,
				   compInfo=compInfo}
		       before checkErrors ()

		 val lambda = C.inline {genLambda = genLambda,
					imports = importPids,
					symenv = symenv}

		 val {lambda_e, lambda_i} =
	                C.split{lambda = lambda, 
                                enable = !Control.lambdaSplitEnable}

		 val new_symenv = C.symDelta (exportPid, lambda_i)

		 val executable =
		       codegen {lambda = lambda_e, compInfo=compInfo}
		       before checkErrors ()
		       (*
			* interp mode is temporarily(?) turned off  
                        *
			* (if !Control.interp
			*  then Interp.interp lambda_e
			*  else codegen {lambda = lambda_e, 
                        *                compInfo=compInfo})
			*)

		 val executable = isolate (interruptable (perform executable))

		 val new_dynenv = let
		      val _ = SMLofNJ.Internals.ProfControl.current
				:= Profile.otherIndex
		      val result = 
			 C.execute{executable=executable,imports=importPids,
				   exportPid=exportPid,dynenv=dynenv}
		    in SMLofNJ.Internals.ProfControl.current := Profile.compileIndex;
		       result
		   end

		 val newEnv = E.concatEnv
		       ({static=newenv, dynamic=new_dynenv,
                         symbolic=new_symenv},
			 #get localEnvRef ())

		   (* refetch localEnvRef because execution may 
		      have changed its contents *)

	      in PrettyPrint.with_pp (#errConsumer source)
		  (fn ppstrm => printer 
		    (E.layerEnv(newEnv,#get baseEnvRefunSC ()))
		    ppstrm (absyn, exportLvars));

		 #set localEnvRef newEnv
	     end

    fun loop() = (oneUnit(); loop())

 in interruptable loop ()
end (* function evalLoop *)

(*** interactive loop, with error handling ***)
fun interact (interactParams) : unit =
    let val source = Source.newSource("stdIn",1,TextIO.stdIn,true,
				       EM.defaultConsumer());
	fun flush'() = 
	    case TextIO.canInput(TextIO.stdIn, 4096)
	      of NONE => ()
	       | (SOME 0) => ()
	       | (SOME _) => (ignore (TextIO.input TextIO.stdIn); flush'())

	fun flush() = (#anyErrors source := false; 
		       flush'() handle IO.Io _ => ())

        local val p1 = Substring.isPrefix "build/evalloop.sml:"
              val p2 = Substring.isPrefix "build/compile.sml:"
              val p3 = Substring.isPrefix "util/stats.sml:"
          in fun edit [s] = [s]
	       | edit nil = nil
               | edit (s::r) = 
	         let val s' = Substring.all s
                  in if p1 s' orelse p2 s' orelse p3 s'
                      then edit r else s :: edit r
                 end
          end

	fun showhist' [s] = say(concat["  raised at: ", s, "\n"])
	  | showhist' (s::r) = (showhist' r; 
			       say (concat["             ", s, "\n"]))
	  | showhist' [] = ()

	fun exnMsg (Compile.Compile s) = concat["Compile: \"", s, "\""]
	  | exnMsg (C.TopLevelException e) = exnMsg e
	  | exnMsg exn = General.exnMessage exn

	fun showhist (C.TopLevelException e) = showhist e
	  | showhist C.SilentException = ()
	  | showhist e = showhist' (edit(SMLofNJ.exnHistory e))

	fun loop () =
	       evalLoop interactParams source
	       handle EndOfFile => ()
		    | Interrupt => (say "\nInterrupt\n"; 
				  flush(); loop())
		    | EM.Error => (flush(); loop())
		    | C.Compile "syntax error" => (flush(); loop())
		    | C.Compile s =>
			(say(concat["\nuncaught exception Compile: \"",
				   s,"\"\n"]);
			 flush(); loop())
		    | C.TopLevelException C.TopLevelCallcc =>
			(say("Error: throw from one top-level expression \
			     \into another\n");
			 flush (); loop ())
		    | C.TopLevelException EM.Error =>
			(flush (); loop ())
		    | C.TopLevelException C.SilentException =>
			(flush (); loop ())
		    | C.TopLevelException exn => let
	                 val msg = exnMsg exn
			 val name = exnName exn
		         in
                           if (msg = name)
			     then say (concat[
		                 "\nuncaught exception ", exnName exn, "\n"
                               ])
			     else say (concat[
		                 "\nuncaught exception ", exnName exn,
                                 " [", exnMsg exn, "]\n"
                               ]);
			   showhist exn;
			   flush(); 
			   loop()
			end
		    | C.SilentException => (flush (); loop ())
		    | exn => (
			 say (concat["\nuncaught exception ", 
				    exnMsg exn, "\n"]);
			 showhist exn;
			 flush(); 
			 loop())

     in loop()
    end (* fun interact *)

fun isTermIn f = 
    let val (rd, buf) = TextIO.StreamIO.getReader(TextIO.getInstream f)
	val isTTY = 
	    case rd
	      of TextPrimIO.RD{ioDesc = SOME iod, ...} =>
		   (OS.IO.kind iod = OS.IO.Kind.tty)
	       | _ => false

	val buf = if (buf = "") then NONE else SOME buf
     in (* since getting the reader will have terminated the stream, we need
	 * to build a new stream. *)
	TextIO.setInstream(f, TextIO.StreamIO.mkInstream(rd, buf));
	isTTY
    end

fun evalStream interactParams 
               (fname:string,stream:TextIO.instream) : unit =
    let val interactive = isTermIn stream
	val source = Source.newSource(fname,1,stream,interactive,
				      EM.defaultConsumer())
     in evalLoop interactParams source
	handle exn =>
	  (Source.closeSource source;
	   case exn 
	   of EndOfFile => () 
	    | C.TopLevelException e => raise e
	    | _ => raise exn)
    end 

end (* top-level local *)
end (* functor EvalLoopF *)

(*
 * $Log: evalloop.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.17  1997/10/01 20:51:06  george
 *   Improved error message with exceptions
 *
 * Revision 1.16  1997/09/22  19:00:36  appel
 * In exception history, don't report build/evalloop.sml (etc.)
 *
 * Revision 1.15  1997/09/22  18:32:29  appel
 * Add clause for TopLevelException(EM.Error) => ...silent...
 *
 * Revision 1.14  1997/09/22  18:05:18  appel
 * Function "interruptable" to fix interaction between Interrupt and "use".
 *
 * Revision 1.12  1997/09/17  17:09:12  appel
 * Better error message for top level callcc
 *
 * Revision 1.11  1997/09/15  15:57:10  dbm
 *   Cleanup and simplification of translate and its type.
 *
 * Revision 1.10  1997/09/04  15:20:59  george
 *   Bug fix for TopLevelException bug. -- appel
 *
 * Revision 1.9  1997/08/25  19:20:03  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.8  1997/08/02  02:11:24  dbm
 *   New top level using Environment.
 *
 * Revision 1.7  1997/06/30  19:37:04  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.6  1997/05/05  19:55:06  george
 *    Turning off some measurement hooks - zsh
 *
 * Revision 1.5  1997/03/25  13:41:43  george
 *   Fixing the coredump bug caused by duplicate top-level declarations.
 *   For example, in almost any versions of SML/NJ, typing
 *           val x = "" val x = 3
 *   would lead to core dump. This is avoided by changing the "exportLexp"
 *   field returned by the pickling function (pickle/picklemod.sml) into
 *   a list of lambdavars, and then during the pretty-printing (print/ppdec.sml),
 *   each variable declaration is checked to see if it is in the "exportLvars"
 *   list, if true, it will be printed as usual, otherwise, the pretty-printer
 *   will print the result as <hiddle-value>.
 * 						-- zsh
 *
 * Revision 1.4  1997/03/19  20:04:37  george
 *   bugfix for 1105 -- message for uncaught exceptions has duplicated "exception"
 *
 * Revision 1.3  1997/03/03  17:10:43  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.2  1997/02/26  21:49:03  george
 *    Fix the BUG 1116 about very slow top-level interactive loop, reported
 *    by Larry Paulson (Dave, if you have HOL with you, can you test this out?)
 *
 *)
