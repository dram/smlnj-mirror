structure M68Glue : sig end = struct

structure M68As =
 struct structure CM = M68CM(M68AsCode)
        fun start s = M68Assem.outfile := open_out(s ^ ".s")
	fun stop () = close_out(!M68Assem.outfile)
 end

structure M68M =
 struct structure CM = M68CM(M68MCode)
	structure B = BasicM68
	val name = ref ""
        fun start s = name := s
	fun stop () = 
	  let val (size,f) = B.finish(B.sizejump,B.emitjump,B.emitlong)
	      val outfile = open_out(!name ^ ".mo")
	      val write = output outfile
	      fun writebyte i = write(chr i)
	  in  f writebyte;
	      close_out outfile
	  end
 end

structure BatchAs = Batch(M68As)
structure BatchM = Batch(M68M)

val pr = output std_out
open PrintUtil

fun skip_white stream =
     case lookahead stream of
	  " " => (input stream 1; skip_white stream)
	| "\t" => (input stream 1; skip_white stream)
	| "\n" => (input stream 1; skip_white stream)
	| _ => ()

fun getword stream =
    let val nextchar = input stream 1
    in  case nextchar of
	  "" => ""
	| " " => ""
	| "\t" => ""
	| "\n" => ""
	| _ => nextchar ^ getword stream
    end

fun appargs function =
	while not (end_of_stream std_in) do (
	    skip_white std_in;
	    if not (end_of_stream std_in)
		then function(getword std_in)
		else ()
	)


(* The commandline interpreter *)
val dir = ref ""
val globalhandle = ref true
val dumpCore = ref false
fun compile s = 
    let val file = !dir ^ s
    in  pr ("[Compiling " ^ file ^ "]\n"); BatchM.compile file
    end
fun assemble s = 
    let val file = !dir ^ s
    in  pr ("[Assembling " ^ file ^ "]\n"); BatchAs.compile file
    end
fun load s = 
    let val file = !dir ^ s
    in  pr ("[Loading " ^ file ^ "]\n"); PreBatch.load file
    end
fun export s = 
    let val file = !dir ^ s
    in  pr("[Exporting to " ^ file ^ "]\n");
	exportML file;
	pr "hello there\n"
    end

exception Notfound_Compile of string
local open System.Control CPSoption
      val flags = [
		("internals",internals),
		("CGoptions.tailrecur",CGoptions.tailrecur),
		("CGoptions.recordopt",CGoptions.recordopt),
		("CGoptions.tail",CGoptions.tail),
		("CGoptions.profile",CGoptions.profile),
		("CGoptions.closureprint",CGoptions.closureprint),
		("CGoptions.hoist",CGoptions.hoist),
		("CGoptions.reduce",CGoptions.reduce),
		("foldconst",foldconst),
		("etasplit",etasplit),
		("comment",comment),
		("alphac",alphac),
		("printsize",printsize),
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
		("debugging",debugging),
		("timings",timings),
		("dumpCore",dumpCore),
		("globalhandle",globalhandle)]

in
fun getflag f =
	let fun get nil = raise Notfound_Compile f
	      | get ((name,flag)::tl) = if f=name then flag else get tl
	in  get flags end

fun printflags () =
      (pr "[Flags:\n";
       app (fn(name,flag:bool ref) =>
		(pr name; pr " = "; print(!flag); pr "\n")) flags;
       pr "]\n")
end

fun toggle "" = printflags()
  | toggle arg =
    let val flag = getflag arg
	val new = not(!flag)
    in  pr ("["^arg^" := "^makestring new^"]\n");
	flag := new
    end

fun lsave () = (toggle "saveLambda"; toggle "saveLvarNames")

fun atoi s =
  let val dtoi = fn "0" => 0 | "1" => 1 | "2" => 2 | "3" => 3 | "4" => 4
		  | "5" => 5 | "6" => 6 | "7" => 7 | "8" => 8 | "9" => 9
		  | _ => (pr "[garbled integer input]\n"; raise PreBatch.Stop)
  in  revfold (fn(a,b) => b * 10 + dtoi a) (explode s) 0
  end
fun gcmessage() =
  let val f = System.Control.Runtime.gcmessages
  in  f := (!f + 1) mod 4;
      pr "[gcmessages := "; print(!f); pr "]\n"
  end
fun summary() =
 (System.Stats.summary();
  pr "freemap ";
  pr(System.Timer.makestring(!FreeMap.freetimer));
  pr "s\n";
  pr(makestring(!CPSoption.knowngen));
  pr " knowngen\n";
  pr(makestring(!CPSoption.stdgen));
  pr " stdgen\n";
  ())
local val execs = [
		("lsave",lsave),
		("summary",summary),
		("prFun",fn () =>
			PreBatch.prFun(atoi(skip_white std_in; getword std_in))),
		("gcmessages",gcmessage),
		("setbodysize",fn () =>
			let val i = atoi(skip_white std_in; getword std_in)
			in  pr "[bodysize := "; print i; pr "]\n";
			    CPSoption.bodysize := i
			end),
		("setreducemore",fn () =>
			let val i = atoi(skip_white std_in; getword std_in)
			in  pr "[reducemore := "; print i; pr "]\n";
			    CPSoption.reducemore := i
			end),
		("printslots",fn () => PreBatch.printslots(skip_white std_in;
							   getword std_in)),
		("dumpMap",PreBatch.dumpMap),
		("asBoot",fn () => (BatchAs.bootEnv true; ())),
		("mBoot",fn () => (BatchM.bootEnv true; ())),
		("reprime",PreBatch.reprime)]
in
fun getexec f =
	let fun get nil = raise Notfound_Compile f
	      | get ((name,exec)::tl) = if f=name then exec else get tl
	in  get execs end

fun printexecs () = (pr "[Available execs:\n";
		     app (fn ("setbodysize",_) =>
				(pr "setbodysize <int> (currently ";
				 Integer.print(!CPSoption.bodysize); pr ")\n")
			   | ("setreducemore",_) =>
				(pr "setreducemore <int> (currently ";
				 Integer.print(!CPSoption.reducemore); pr ")\n")
			   | ("prFun",_) => pr "prFun <lvar>\n"
			   | ("printslots",_) => pr "printslots <structure>\n"
			   | (name,_) => (pr name; pr "\n")) execs;
		     pr "]\n")
end

fun execute "" = printexecs()
  | execute arg =
    let val exec = getexec arg
    in  pr("["^arg^"()]\n");
	exec()
    end

fun help() = (pr "\
\!file      => compile the file.\n\
\*file      => assemble the file.\n\
\<file      => parse the file.\n\
\>file      => export to a file.\n\
\%          => print the last generated lambda.\n\
\#word      => comment; ignored.\n\
\@directory => look for files in a directory.  directory should end in /.\n\
\~function  => execute a function.\n\
\^flag      => toggle a flag.\n\
\?          => print this help message.\n"; ())

fun interp "" = ()
  | interp word =
    let val arg = substring(word,1,length word - 1) handle Substring => ""
    in  (case substring(word,0,1) of
 	      "!" => compile arg
	    | "*" => assemble arg
	    | "<" => load arg
	    | ">" => export arg
	    | "%" => PreBatch.prLambda()
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

fun interp1 word = case !globalhandle of
	  true => (interp word
		   handle PreBatch.Stop => (pr "[Failed on ";
					pr_mlstr word;
					pr "]\n";
					flush_out std_out)
			| e => (pr "[Failed on ";
				pr_mlstr word; pr " with ";
				pr(System.exn_name e); pr "]\n";
				flush_out std_out))
	| false => interp word handle e =>
		(pr "[Failed on ";
		 pr_mlstr word; pr " with ";
		 pr(System.exn_name e); pr "]\n";
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

(* Compile the pervasives, generating Math.mo and PervFunc.mo *)
val _ = BatchM.bootEnv false
val _ = pr "hello there\n"

(* Start up the command-line interpreter. *)
val _ = appargs interp1


end (* structure Glue *)
