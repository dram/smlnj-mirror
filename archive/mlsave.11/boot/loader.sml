signature Startup =
 sig val name : string
 end

functor Loader ( S : Startup ) : sig end =
struct

 open System

 val dict : (string*object) list ref = ref [("Initial",!pstruct)]

 exception Notfound_Loader

 fun lookup s =
    let fun f ((s1,stru)::r) = if s=s1 then stru else f r
	  | f nil = raise Notfound_Loader
     in f (!dict)
    end

 fun enter pair = dict := pair::(!dict)

 fun getstruct s =
    lookup s handle Notfound_Loader =>
    let val _ = (print "[Loading "; print s; print "]\n"; flush_out(std_out))
	val g = System.readfile ("mo/" ^ s ^ ".mo");
        val (exec,sl) = boot g ()
        val structs = map getstruct sl
        val _ = (print "[Executing "; print s; print "]\n"; flush_out(std_out))
        val str = exec structs
     in enter (s,str);
	str
    end

    val _ = (getstruct S.name; close_out std_out)

end
