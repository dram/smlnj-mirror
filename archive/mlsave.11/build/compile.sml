(* compile.sml *)

functor Compile (Glue : sig val compile : string -> unit    
			    val assemble : string -> unit
			    val load : string -> unit
			    val bootEnv : unit -> unit
			    val prLambda : unit -> unit
			    val saveLambda : bool ref
			    val reset : unit -> unit
			    val dumpMap : unit -> unit
			    exception Stop
			end) : sig end
 = struct

open PrintUtil

fun skip_white stream =
     case lookahead stream of
	  " " => (input(stream,1); skip_white stream)
	| "\t" => (input(stream,1); skip_white stream)
	| "\n" => (input(stream,1); skip_white stream)
	| _ => ()


fun getword stream =
    let val nextchar = input(stream,1)
     in case nextchar of
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


(* Compile the pervasives *)
val _ = Glue.bootEnv()
val _ = print "hello there\n"


(* debugging aid--print the slots of a structure *)
local open Basics Access
in
fun printslot {name,access=SLOT s} =
	(print "Slot "; print s; print " : ";
	 print(Symbol.name name);
	 print "\n"; ())
  | printslot {name,access=LVAR s} =
	(print "Lvar "; print s; print " : ";
	 print(Symbol.name name);
	 print "\n"; ())
  | printslot {name,access=INLINE s} =
	(print "Inline "; print s; print " : ";
	 print(Symbol.name name);
	 print "\n"; ())
  | printslot {name,access=PATH _} =
	(print "Path?? :";
	 print(Symbol.name name);
	 print "\n"; ())

val usl : {name:Symbol.symbol,access:access} list ref = ref nil

fun buildlist (_,VARbind(VALvar{name,access,...})) =
		usl := {name=name,access=access} :: !usl
  | buildlist (_,STRbind(STRvar{name,access,...})) =
		usl := {name=name,access=access} :: !usl
  | buildlist (_,CONbind(DATACON{name,rep=ref(VARIABLE access),...})) =
		usl := {name=name,access=access} :: !usl
  | buildlist _ = ()

fun slotgt ({access=SLOT s1,name},{access=SLOT s2,name=_}) = s1 > s2
  | slotgt ({access=SLOT _,...},_) = true
  | slotgt ({access=LVAR v1,...},{access=LVAR v2,...}) = v1 > v2
  | slotgt ({access=LVAR _,...},_) = true
  | slotgt ({access=INLINE i1,...},{access=INLINE i2,...}) = i1 > i2
  | slotgt ({access=INLINE _,...},_) = true
  | slotgt _ = ErrorMsg.impossible "Path access in printslots"

fun printslots () =
    let val s = (skip_white std_in; getword std_in)
	val STRvar{binding=STRstr{table,...},...} =
		EnvAccess.lookSTR(SymbolTable.stringToSymbol s)
	val unsortedlist = (usl := nil; Table.app(table,buildlist); !usl)
	val sortedlist = Sort.sort slotgt unsortedlist
    in
	app printslot sortedlist
    end
    handle Bind => ErrorMsg.impossible "Weird structure in printslots"
end (* local open Basics Access *)

(* The commandline interpreter *)
val dir = ref ""
val globalhandle = ref true
fun compile s = 
    let val file = !dir ^ s
    in
	print ("[Compiling " ^ file ^ "]\n");
	Glue.compile file
    end
fun assemble s = 
    let val file = !dir ^ s
    in
	print ("[Assembling " ^ file ^ "]\n");
	Glue.assemble file
    end
fun load s = 
    let val file = !dir ^ s
    in
	print ("[Loading " ^ file ^ "]\n");
	Glue.load file
    end
fun export s = 
    let val file = s
    in
	print("[Exporting to " ^ file ^ "]\n");
	exportML file;
	output(std_out,"hello there\n")
    end

exception Notfound_Compile of string
local open System.Control
      val flags = [
		("Basics.internals",Basics.internals),
		("CGoptions.knownfunc",CGoptions.knownfunc),
		("CGoptions.tailrecur",CGoptions.tailrecur),
		("CGoptions.primapp",CGoptions.primapp),
		("CGoptions.recordopt",CGoptions.recordopt),
		("CGoptions.tail",CGoptions.tail),
		("CGoptions.closurecount",CGoptions.closurecount),
		("CGoptions.closureprint",CGoptions.closureprint),
		("CGoptions.chained",CGoptions.chained),
		("CGoptions.hoist",CGoptions.hoist),
		("MC.printArgs",MC.printArgs),
		("MC.printRet",MC.printRet),
		("MC.bindContainsVar",MC.bindContainsVar),
		("MC.bindExhaustive",MC.bindExhaustive),
		("MC.matchExhaustive",MC.matchExhaustive),
		("MC.matchRedundant",MC.matchRedundant),
		("Access.saveLvarNames",Access.saveLvarNames),
		("Glue.saveLambda",Glue.saveLambda),
		("debugging",debugging),
		("M68options.trapv",M68options.trapv),
		("Runtime.gcmessages",Runtime.gcmessages),
		("globalhandle",globalhandle)]

in
fun getflag f =
	let fun get nil = raise Notfound_Compile f
	      | get ((name,flag)::tl) = if f=name then flag else get tl
	in  get flags end

fun printflags () =
	let fun p nil = ()
	      | p ((name:string,_)::tl) = (print name; print "\n"; p tl)
	in  print "[Available flags:\n";
	    p flags;
	    print "]\n";
	    ()
	end
end

fun toggle "" = printflags()
  | toggle arg =
    let val flag = getflag arg
	val new = not(!flag)
    in
	print ("["^arg^" := "^makestring new^"]\n");
	flag := new
    end

fun lsave () = (toggle "Glue.saveLambda"; toggle "Access.saveLvarNames")

local  val execs = [
		("lsave",lsave),
		("printslots",printslots),
		("Glue.dumpMap",Glue.dumpMap),
		("Glue.reset",Glue.reset)]
in
fun getexec f =
	let fun get nil = raise Notfound_Compile f
	      | get ((name,exec)::tl) = if f=name then exec else get tl
	in  get execs end

fun printexecs () =
	let fun p nil = ()
	      | p ((name:string,_)::tl) = (print name; print "\n"; p tl)
	in  print "[Available execs:\n";
	    p execs;
	    print "]\n";
	    ()
	end
end

fun execute "" = printexecs()
  | execute arg =
    let val exec = getexec arg
    in
	print ("["^arg^"()]\n");
	exec()
    end

fun help() = (print "\
\!file       => compile the file.\n\
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
    in
	(case substring(word,0,1) of
 	      "!" => compile arg
	    | "*" => assemble arg
	    | "<" => load arg
	    | ">" => export arg
	    | "%" => Glue.prLambda()
	    | "#" => ()			(* comment *)
	    | "@" => dir := arg		(* change load directory *)
	    | "~" => execute arg	(* execute function *)
	    | "^" => toggle arg		(* toggle flag *)
	    | "?" => help()		
	    |  _  => output(std_out,"[What is \""^word^"\"?]\n")
	) handle e as Notfound_Compile f =>
		   (output(std_out,"[flag \""^f^"\" not recognized]\n");
		    raise e)
    end

fun interp1 word = case !globalhandle of
	  true => (interp word
		   handle Glue.Stop => (print "[Failed on ";
					pr_mlstr word;
					print "]\n";
					flush_out std_out)
			| e => (print "[Failed on ";
				pr_mlstr word; print " with ";
				print(System.exn_name e); print "]\n";
				flush_out std_out))
	| false => interp word handle e => (flush_out std_out; raise e)

val _ = appargs interp1


end (* structure Compile *)
