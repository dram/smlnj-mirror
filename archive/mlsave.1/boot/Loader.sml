signature Startup =
 sig val name : string
 end

functor Loader ( S : Startup ) =
struct

 open Boot

 val dict : (string*Structure) list ref = ref [("Initial",!pstruct)]
 exceptionx notfound

 fun lookup s =
    let fun f ((s1,stru)::r) = if s=s1 then stru else f r
	  | f nil = raisex notfound
     in f (!dict)
    end

 fun enter pair = dict := pair::(!dict)

 val BLOCK = 512

(* fun readfile f : string =
  let fun read n =
	if n > BLOCK
	    then let val n2 = n div 2
		     val first = read n2
		  in if length(first)=n2 then first^(read n2) else first
		 end
	    else input(f,n)
      fun try n = let val first = read n
		   in if length(first)=n then first^try(n+n) else first
		  end
   in try BLOCK
  end
*)
 fun getstruct s =
    lookup s handlex notfound =>
    let val _ = (print "[Loading "; print s; print "]\n"; flush_out(std_out))
	val g = readfile ("mo/" ^ s ^ ".mo");
        val (exec,sl) = boot g ()
        val structs = map getstruct sl
        val _ = (print "[Executing "; print s; print "]\n"; flush_out(std_out))
        val str = exec structs
     in enter (s,str);
	str
    end

    val _ = (getstruct S.name; close_out std_out)

end
