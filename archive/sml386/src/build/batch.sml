(* Copyright 1989 by AT&T Bell Laboratories *)
functor Batch(structure M: CODEGENERATOR and A:ASSEMBLER)  : sig end =
struct

val pr = outputc std_out
open PrtUtil PrcFile
structure CGoptions = System.Control.CG

(* command parsing *)

fun skip_white stream =
    case lookahead stream
      of " " => (input(stream,1); skip_white stream)
       | "\t" => (input(stream,1); skip_white stream)
       | "\n" => (input(stream,1); skip_white stream)
       | _ => ()

fun getword stream =
    let val nextchar = input(stream,1)
     in case nextchar
	  of "" => ""
	   | " " => ""
	   | "\t" => ""
	   | "\n" => ""
	   | _ => nextchar ^ getword stream
    end

(* The commandline interpreter *)

val dir = ref ""
val globalhandle = ref true
val dumpCore = ref false

fun compile fname = 
    let val file = !dir ^ fname
        fun p(function,s) = 
	    let fun complain _ s = 
			(print(file ^ ": " ^ s ^ "\n"); raise PrcFile.Stop)
		val code = M.generate(function,NONE,complain)
		val outfile = open_out(s ^ ".mo")
	     in outputc outfile code; close_out outfile
	    end
     in pr ("[Compiling " ^ file ^ "]\n"); process(file, SOME p)
    end

fun assemble s = 
    let val file = !dir ^ s
        fun p(function,s) = 
	    let fun complain _ s = (print(file ^ ": "^s); raise PrcFile.Stop)
		val f = open_out(s ^ ".s")
	     in A.generate ((function,NONE,complain), f);
		 close_out f
	    end
     in pr ("[Assembling " ^ file ^ "]\n"); process(file, SOME p)
    end

fun load s = 
    let val file = !dir ^ s
    in pr ("[Loading " ^ file ^ "]\n"); PrcFile.load file
    end

fun export s = let
      val file = !dir ^ s
      val msg = System.version ^ " (batch compiler)\n"
      in
	pr("[Exporting to " ^ file ^ "]\n"); exportML file; pr msg
      end

exception Notfound_Compile of string
local open System.Control 
      open CG Profile
      val flags = [
		("tailrecur",tailrecur),
		("recordopt",recordopt),
		("tail",tail),
		("profile",profile),
		("closureprint",closureprint),
		("cpsopt",cpsopt),
		("path",path),
		("betacontract",betacontract),
		("eta",eta),
		("selectopt",selectopt),
		("dropargs",dropargs),
		("deadvars",deadvars),
		("flattenargs",flattenargs),
		("switchopt",switchopt),
		("handlerfold",handlerfold),
		("branchfold",branchfold),
		("arithopt",arithopt),
		("betaexpand",betaexpand),
		("hoistup",hoistup),
		("hoistdown",hoistdown),
		("recordcopy",recordcopy),
		("tagopt",tagopt),
		("machdep",machdep),
		("misc1",misc1),
		("misc2",misc2),
		("hoist",hoist),
		("argrep",argrep),
		("reduce",reduce),
		("alphac",alphac),
		("comment",comment),
		("foldconst",foldconst),
		("etasplit",etasplit),
		("printit",printit),
		("printsize",printsize),
		("scheduling",scheduling),
		("internals",internals),
		("MC.printArgs",MC.printArgs),
		("MC.printRet",MC.printRet),
		("MC.bindContainsVar",MC.bindContainsVar),
		("MC.bindExhaustive",MC.bindExhaustive),
		("MC.matchExhaustive",MC.matchExhaustive),
		("MC.matchRedundant",MC.matchRedundant),
		("MC.expandResult",MC.expandResult),
		("saveLvarNames",Access.saveLvarNames),
		("saveLambda",saveLambda),
                ("printit",printit),
                ("markabsyn",markabsyn),
		("debugging",debugging),
		("debugLook",debugLook),
		("debugBind",debugBind),
		("timings",timings),
		("dumpCore",dumpCore),
		("globalhandle",globalhandle),
		("profiling",profiling)]
in
fun getflag f =
    let fun get nil = raise Notfound_Compile f
	  | get ((name,flag)::tl) = if f=name then flag else get tl
     in get flags
    end

fun printflags () =
    (pr "[Flags:\n";
     app (fn(name,flag:bool ref) => (pr name; pr " = "; print(!flag); pr "\n"))
	 flags;
     pr "]\n")
end

fun toggle "" = printflags()
  | toggle arg =
    let val flag = getflag arg
	val new = not(!flag)
    in pr ("["^arg^" := "^makestring new^"]\n"); flag := new
    end

fun lsave () = (toggle "saveLambda"; toggle "saveLvarNames")

fun atoi s =
    let val dtoi = fn "0" => 0 | "1" => 1 | "2" => 2 | "3" => 3 | "4" => 4
		    | "5" => 5 | "6" => 6 | "7" => 7 | "8" => 8 | "9" => 9
		    | _ => (pr "[garbled integer input]\n"; raise PrcFile.Stop)
    in case explode s
	of "~" :: s' => ~ (revfold (fn(a,b) => b * 10 + dtoi a) s' 0)
	 | s' => revfold (fn(a,b) => b * 10 + dtoi a) s' 0
    end

fun gcmessage() =
    let val f = System.Control.Runtime.gcmessages
    in f := (!f + 1) mod 4; pr "[gcmessages := "; print(!f); pr "]\n"
    end

fun summary() =
    (System.Stats.summary();
     pr(makestring(!System.Control.CG.knowngen));
     pr " knowngen\n";
     pr(makestring(!System.Control.CG.knowncl));
     pr " knowncl\n";
     pr(makestring(!System.Control.CG.stdgen));
     pr " stdgen\n";
     ())

val execs =
       [("lsave",lsave),
	("summary",summary),
	("prFun",fn () =>
		PrcFile.prFun(atoi(skip_white std_in; getword std_in))),
	("gcmessages",gcmessage),
	("setratio",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[ratio := "; print i; pr "]\n";
		    System.Control.Runtime.ratio := i
		end),
	("setmaxregs",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[maxregs := "; print i; pr "]\n";
		    System.Control.CG.maxregs := i
		end),
	("setmisc3",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[misc3 := "; print i; pr "]\n";
		    System.Control.CG.misc3 := i
		end),
	("setmisc4",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[misc4 := "; print i; pr "]\n";
		    System.Control.CG.misc4 := i
		end),
	("setknowngen",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[knowngen := "; print i; pr "]\n";
		    System.Control.CG.knowngen := i
		end),
	("setstdgen",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[stdgen := "; print i; pr "]\n";
		    System.Control.CG.stdgen := i
		end),
	("setknowncl",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[knowncl := "; print i; pr "]\n";
		    System.Control.CG.knowncl := i
		end),
	("setsoftmax",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[softmax := "; print i; pr "]\n";
		    System.Control.Runtime.softmax := i
		end),
	("setbodysize",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[bodysize := "; print i; pr "]\n";
		    System.Control.CG.bodysize := i
		end),
	("setrounds",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[rounds := "; print i; pr "]\n";
		    System.Control.CG.rounds := i
		end),
	("setreducemore",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[reducemore := "; print i; pr "]\n";
		    System.Control.CG.reducemore := i
		end),
	("setclosureStrategy",fn () =>
		let val i = atoi(skip_white std_in; getword std_in)
		in  pr "[closureStrategy := "; print i; pr "]\n";
		    CGoptions.closureStrategy := i
		end),
	("printslots",fn () => PrcFile.printslots(skip_white std_in;
						   getword std_in)),
	("flushstdout",fn () => set_term_out(std_out,true)),
	("dumpMap",PrcFile.dumpMap),
	("asBoot",fn () => (PrcFile.bootEnv assemble; ())),
	("mBoot",fn () => (PrcFile.bootEnv compile; ())),
	("primeEnv",PrcFile.primeEnv),
	("clear",System.Control.Profile.clear),
	("reset",System.Control.Profile.reset),
	("report",fn () => System.Control.Profile.report std_out),
	("profileOff",System.Control.Profile.profileOff),
	("profileOn",System.Control.Profile.profileOn)]

fun getexec f =
    let fun get nil = raise Notfound_Compile f
	  | get ((name,exec)::tl) = if f=name then exec else get tl
     in get execs
    end

fun printexecs () =
    (pr "[Available execs:\n";
     app (fn ("setbodysize",_) =>
	       (pr "setbodysize <int> (currently ";
	        Integer.print(!System.Control.CG.bodysize); pr ")\n")
	   | ("setreducemore",_) =>
	       (pr "setreducemore <int> (currently ";
		Integer.print(!System.Control.CG.reducemore); pr ")\n")
	   | ("setclosureStrategy",_) =>
	       (pr "setclosureStrategy <int> (currently ";
		Integer.print(!CGoptions.closureStrategy);
		pr ")\n")
	   | ("prFun",_) => pr "prFun <lvar>\n"
	   | ("printslots",_) => pr "printslots <structure>\n"
	   | (name,_) => (pr name; pr "\n"))
	 execs;
     pr "]\n")

fun execute "" = printexecs()
  | execute arg =
    let val exec = getexec arg
    in  pr("["^arg^"()]\n");
	exec()
    end

fun help() = pr "\
\!file      => compile the file.\n\
\*file      => assemble the file.\n\
\<file      => parse the file.\n\
\>file      => export to a file.\n\
\%          => print the last generated lambda.\n\
\#word      => comment; ignored.\n\
\@directory => look for files in a directory.  directory should end in /.\n\
\~function  => execute a function.\n\
\^flag      => toggle a flag.\n\
\?          => print this help message.\n"

fun interp "" = ()
  | interp word =
    let val arg = substring(word,1,size word - 1) handle Substring => ""
    in  (case substring(word,0,1) of
 	      "!" => compile arg
	    | "*" => assemble arg
	    | "<" => load arg
	    | ">" => export arg
	    | "%" => PrcFile.prLambda()
	    | "#" => ()			(* comment *)
	    | "@" => dir := arg		(* change load directory *)
	    | "~" => execute arg	(* execute function *)
	    | "^" => toggle arg		(* toggle flag *)
	    | "?" => help()		
	    |  _  => pr ("[What is \""^word^"\"?]\n")
	) handle e as Notfound_Compile f =>
		   (pr("[flag \""^f^"\" not recognized]\n");
		    raise e)
    end

exception INTERRUPT
fun setcont () = (
      System.Unsafe.toplevelcont := callcc(fn k => (
      callcc(fn k' => (throw k k'));
      raise INTERRUPT)))
fun interp1 word = if !globalhandle
      then (setcont(); interp word)
      handle PrcFile.Stop => (
              pr "[Failed on "; pr_mlstr word; pr "]\n"; flush_out std_out)
           | ErrorMsg.Cascade s => (
              pr "Cascade: "; print s; pr "\n";
              pr "[Failed on "; pr_mlstr word; pr "]\n"; flush_out std_out)
           | INTERRUPT => (pr "\n[Interrupt]\n"; flush_out std_out)
           | e => (
              pr "[Failed on "; pr_mlstr word; pr " with ";
              pr(System.exn_name e); pr "]\n"; flush_out std_out)
      else (setcont(); interp word)
      handle e => (
          case e
           of INTERRUPT => (pr "\n[Interrupt]\n")
            | _ => (pr "[Failed on "; pr_mlstr word; pr " with ";
                    pr(System.exn_name e); pr "]\n");
	    flush_out std_out;
	    if !dumpCore
	    then (toggle "globalhandle";
		  toggle "dumpCore";
                  pr "[Saving state]\n[Exporting to sml.save]\n";
                  flush_out std_out;
                  if exportML "sml.save"
                    then pr "hello there\n"
                    else (summary(); raise e))
            else raise e)
		 
(* command-line interpreter top-level loop *)
fun toplevel () =
    if end_of_stream std_in
    then ()
    else (skip_white std_in;
	  if (end_of_stream std_in)
	  then () 
	  else (interp1(getword std_in); toplevel ()))

(* load the pervasives (no .mo files generated) *)
val _ = PrcFile.bootEnv load

(* start up command interpreter *)
val _ = (pr "hello there\n"; toplevel ())

end
