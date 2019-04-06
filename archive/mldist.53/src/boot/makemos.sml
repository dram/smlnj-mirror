(* Copyright 1989 by AT&T Bell Laboratories *)
structure MakeMos = 
struct

fun makeMos modname = 
 let type object = System.Unsafe.object
     val boot : string -> unit ->
	    ((object list -> (object * ByteArray.bytearray array)) * string list)
	  = System.Unsafe.boot

     val dict = ref ["Core"]

     fun lookup s = let fun f (s1::r) = s=s1 orelse f r
		          | f nil = false
		     in f (!dict)
		    end

     fun enter s = dict := s::(!dict)

     fun readfile s =
	let val stream = open_in s
	    val file = input stream (can_input stream)
	in  close_in stream;
	    System.Unsafe.flush_cache file;
	    file
	end

     val f = open_out ("runtime/" ^ modname ^ ".mos")
     val say = output f
     fun getstruct s = if lookup s then ()
		else let val s' = "mo/" ^ s ^ ".mo"
			 val _ = (say s'; say "\n")
			val g = readfile s'
		        val (_,sl) = boot g ()
		    in  app getstruct sl;
			enter s
		    end
  in output std_out (modname ^ ".mos\n");
     say "mo/CoreFunc.mo\n";
     app getstruct ["Initial","Loader",modname];
     close_out f
 end

end;

app MakeMos.makeMos ["IntNull","IntVax","CompVax","IntM68","CompM68",
    "IntSparc","CompSparc","IntNS32","CompNS32",
	"CompMipsLittle", "IntMipsLittle", "CompMipsBig", "IntMipsBig"];
     
