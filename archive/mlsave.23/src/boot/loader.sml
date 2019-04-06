signature Startup =
 sig 
     val core : System.object
     val initial : System.object
     val math : System.object
     val name : string
 end

functor Loader ( S : Startup ) : sig end =
struct

 open System
 val boot : string ->
	    unit ->
	    ((object list -> (object * ByteArray.bytearray array)) * string list)
	  = System.boot

 val dict : (string*object) list ref = 
	ref [("Initial",S.initial),("Core",S.core),("NewMath",S.math)]

 val _ = System.pstruct := {core=S.core,math=S.math,initial=S.initial}

 exception Notfound_Loader

 fun lookup s =
    let fun f ((s1,stru)::r) = if s=s1 then stru else f r
	  | f nil = raise Notfound_Loader
     in f (!dict)
    end

 fun enter pair = dict := pair::(!dict)

 fun readfile s =
	let val stream = open_in s
	    val file = input stream (can_input stream)
	in  close_in stream;
	    file
	end

 fun getmo s =
    let fun f DATANIL = readfile s
	  | f (DATACONS(s',t,x)) = if s=s' then t else f x
     in f System.datalist
    end

 fun getstruct s =
	lookup s handle Notfound_Loader =>
	    let val _ = (print "[Loading "; print s; print "]\n")
		val g = getmo ("mo/" ^ s ^ ".mo");
	        val (exec,sl) = boot g ()
	        val structs = map getstruct sl
	        val _ = (print "[Executing "; print s; print "]\n")
	        val (str,profile) = exec structs
	    in  enter (s,str);
		System.Control.Profile.add profile;
		str
	    end
 val pr = output std_out
 val _ = (getstruct S.name; System.cleanup())
	    (* this is the global exception handler of the sml system *)
	    handle Io_failure s =>
		     (pr "uncaught Io_failure (Loader): ";
		      pr s;
		      pr "\n";
		      System.cleanup())
		 | SystemCall s =>
		     (pr "uncaught SystemCall (Loader): ";
		      pr s;
		      pr "\n";
		      System.cleanup())
		 | exn =>
		     (pr "uncaught exception (Loader): ";
		      pr (exn_name exn);
		      pr "\n";
		      System.cleanup())

end (* functor Loader *)
