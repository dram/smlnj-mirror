functor Reopen(structure Machm : CODEGENERATOR)
		: sig val instrument: Lambda.lexp -> Lambda.lexp end =
struct
open  Basics Access Lambda
structure U = System.Unsafe
type object= U.object
val cast = U.cast

structure DA = Dynamic(struct open Array
			      type array=lexp Array.array
			      type elem = lexp
			end)

val saved = DA.array(RECORD nil)
val magiccount = ref 0

exception BadReal of string

fun codegen lambda =
	let val _ = (MCprint.printLexp lambda; print "\n")
	    fun complain _ s = raise BadReal s
	    val code = Machm.generate(lambda,complain)
	 in U.CInterface.flush_cache code;
            (U.boot : string -> (int->object) -> object) code
	end

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)

fun gettag obj = U.int (U.tuple obj sub 1)

exception Switch
exception Env
     
fun switch(obj,cl,default) =
    let fun try ((INTcon i, e)::r) = if (U.int obj = i handle U.Boxity=>false)
					then e else try r
	  | try ((REALcon _, e)::r) = raise Env
	  | try ((STRINGcon s, e)::r) = if (U.string obj = s
						 handle U.Boxity=>false)
					then e else try r
	  | try((DATAcon(DATACON{rep,...}),e)::r) =
	    (case rep
	       of TAGGED i => if (gettag obj = i handle U.Boxity => false)
			      then e else try r
		| CONSTANT i => if (U.int obj = i handle U.Boxity => false)
				then e else try r
		| TRANSPARENT =>
			    if ((U.tuple obj; true) handle U.Boxity => false)
			    then e else (try r handle Switch => e)
		| TRANSB => if ((U.tuple obj; true) handle U.Boxity => false)
			    then e else try r
		| TRANSU => if ((U.int obj; true) handle U.Boxity => false)
			    then e else try r
		| REF => e
		| _ => ErrorMsg.impossible "reopen.switch: funny datacon")
	  | try nil = case default
			of SOME e => e
			 | NONE => raise Switch
    in  try cl
	handle Switch => 
	  ErrorMsg.impossible "reopen.switch: none of the datacons matched"
    end

fun delayAnalyze lexp =
 let val fv = Opt.freevars lexp
     val env = mkLvar()
     fun bind(i,nil) = lexp
       | bind(i,a::r) = APP(FN(a, bind(i+1,r)), SELECT(i, VAR env))
     val magic = !magiccount
  in magiccount := magic+1;
     DA.update(saved,magic,FN(env, bind(1,fv)));
     print "magic = "; print magic; print "\n";
     MCprint.printLexp(DA.sub(saved,magic)); print "\n";
     (*APP(PRIM P.delay, 
	 RECORD[INT(System.Tags.tag_suspension div 2),
		    RECORD(INT magic :: map VAR fv)])*)
     RECORD(INT magic :: map VAR fv)
 end

fun forcer (closure) =
 let val m : object Intmap.intmap = Intmap.new(32,Env)
     fun enter obj = let val v = mkLvar() in Intmap.add m (v,obj); VAR v end
     val get = Intmap.map m
     fun undelay (a: object array) =
	let val e = 
           APP(Opt.alphaConvert(DA.sub(saved,((cast (a sub 0)):int))), 
				enter(cast a))
         in print "undelay:\n"; MCprint.printLexp e; print "\n"; e
        end
     fun eval lexp =
        SOME(case lexp
              of SELECT(i,VAR v) => enter(cast(get v) sub i) before print "*1"
               | SWITCH(VAR v,cel,default) => switch(get v, cel,default)
before print "*2"
               | APP(PRIM P.force, APP(PRIM P.delay,b)) => b
before print "*3"
	       | APP(PRIM P.force, VAR v) => undelay(cast(get v))
before print "*4"
	       | VAR v => let val v' = get v
			   in if System.Unsafe.boxed v' then raise Env
				else INT(cast v')
before print "*5"
			   end
	       | APP(PRIM P.alength, VAR v) => INT(Array.length(cast(get v)))
before print "*6"
	       | APP(PRIM P.slength, VAR v) => INT(String.size(cast(get v)))
before print "*7"
	       | APP(PRIM P.ordof, RECORD[VAR v,VAR w]) => 
			INT(String.ordof(cast(get v),cast(get w))
			     handle Ord => raise Env)
before print "*8"
	       | _ => raise Env)
         handle Env => NONE
     val reduce = Opt.greduce eval
     fun looker i = get i handle Env => System.Unsafe.lookup i
     fun package lexp = codegen (Opt.closetop(lexp,ProcessFile.getCore())) looker
  in package(instrument(reduce(undelay closure)))
 end
	    
and instrument lexp =
 let val forcerpath = translatepath(!CoreInfo.forcerPath)
     val _ = System.Unsafe.forcer_p := cast forcer
     fun f(APP(PRIM(P.delay),b)) = delayAnalyze b
       | f(PRIM P.delay) = let val v = mkLvar()
			     in f(FN(v,APP(PRIM(P.delay),VAR v)))
			    end
       | f(PRIM P.force) = APP(PRIM P.!,forcerpath)
       | f(APP(a,b)) = APP(f a, f b)
       | f(FN(v,b)) = FN(v, f b)
       | f(FIX(vl,el,e)) = FIX(vl,map f el, f e)
       | f(SWITCH(e,cel,SOME d)) = SWITCH(f e, map f2 cel, SOME(f d))
       | f(SWITCH(e,cel,NONE)) = SWITCH(f e, map f2 cel, NONE)
       | f(RECORD el) = RECORD(map f el)
       | f(SELECT(i,e)) = SELECT(i, f e)
       | f(RAISE e) = RAISE(f e)
       | f(HANDLE(a,b)) = HANDLE(f a, f b)
       | f e = e
     and f2(c,e) = (c, f e)
  in let val z = f lexp
      in (MCprint.printLexp z; print "\n"); z
     end
 end

end
