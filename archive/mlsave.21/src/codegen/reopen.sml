signature SYS =
   sig	type object
	structure ByteArray : BYTEARRAY
        datatype value = RECORDval of int * object
		       | ARRAYval of object array
		       | STRINGval of string
		       | BYTEARRAYval of ByteArray.bytearray
		       | CLOSUREval of object * int
		       | INTval of int
		       | OTHERval of object
        val getvalue : 'a -> value
        val followpath : (object * int list) -> object
      (* the interpretation of paths is as in Codenv; that is, 
         [a,b,c] has only two fetches. *)
   end

functor Reopen(A : sig structure C : CODEGEN and Machm : MACHINECODE end)
	 : sig end =
struct

open A A.C Access Basics Lambda A.C.Codenvl ErrorMsg

structure Sys : SYS = System
open Sys

    fun gencode lambda =
       let val (size,emit) = (C.generate lambda;
			      Machm.assemble())
	   val a = System.create_s(size+4)
	   val pos = ref 0
	   fun writebyte i = (System.store_s(a,!pos,i); inc pos)
       in  emit writebyte;
	   (System.boot : string -> ((int->object) -> 'a -> 'b)) a
       end

val pure = Opt.pure



fun reopen (arg : 'a -> 'b) =
let val _ = print "reopening!\n"
    exception Reopenmap
    val free : object Intmap.intmap = Intmap.new Reopenmap
    val addvalue = Intmap.add free
    fun getvar v = SOME(getvalue(Intmap.map free v))
		    handle Reopenmap => NONE


fun uncompile obj =
   case getvalue obj 
    of INTval i => INT i
     | _ =>  let val x = mkLvar()
              in addvalue(x,obj); VAR x
             end

fun openup (obj, id) =
  let val (exp,env) = reOpenLookup id
      fun findfree 0 = VAR 0
	| findfree v =
	(case Codenvl.lookup env v
         of PATH(0::l) => uncompile(followpath(obj,l))
          | _ => impossible "funny access in re-open"
	  )
     in Opt.mapfree findfree exp
    end

fun subst(v,e) = Opt.mapfree (fn x => if v=x then e else VAR x)

fun reduce env =
 let fun red e =
 case e
  of VAR v => env v
   | FN (v,b) => FN(v, red b)
   | APP(f,a8) =>
       (case red f
	 of f' as FN(v,b) => 
	      if pure a8
	       then reduce(fn w => if w=v then Opt.mapfree VAR a8 else env w) b
	       else APP(f',red a8)
	  | f' as VAR w => 
	       (case getvar w
		 of SOME(CLOSUREval clo) =>
		    let val f'' = openup clo
		     in MCprint.printLexp f'; print " == ";
			MCprint.printLexp f''; print "\n";
		        red(APP(f'', a8))
		    end
(*		    if pure a8
		     then red(APP(openup clo, a8))
		     else APP(f', red a8)
*)	          | _ => APP(f', red a8))
	  | f' => APP(f', red a8))
   | RECORD l => RECORD(map red l)
   | SELECT(i,RECORD l) => red(nth(l,i))
   | SELECT(i, x) => 
	   (case red x
	     of RECORD l => nth(l,i)
	      | x' as VAR w => (case getvar w
			   of SOME(RECORDval (n, obj)) => 
				uncompile(followpath(obj, [i,0]))
			    | _ => SELECT(i, x'))
	      | a7 => SELECT(i,a7))
   | RAISE x => RAISE(red x)
   | SWITCH(e, l, d) => 
     let val e' = red e
	 fun giveup() =  SWITCH(e', map(fn(c,x)=>(redcon c,red x)) l,
			    case d of SOME d' => SOME(red d') | NONE => NONE)
	 fun try x = red(Opt.switch(SWITCH(x, l, d)))
      in case e'
       of VAR v =>
	    (case getvar v
	      of SOME(RECORDval(2,obj)) =>
		       (case getvalue(followpath(obj,[1,0]))
			 of INTval i => try(RECORD[INT i,INT 0])
			  | _ => try(RECORD[INT 0]))
	       | SOME(RECORDval(n,obj)) => try(RECORD[INT 0])
	       | SOME(STRINGval s) => try(STRING s)
	       | SOME(INTval i) => try(INT i)
	       | _ => giveup())
       | INT _ => try e'
       | RECORD[INT i,_] => try e'
       | RECORD[VAR v,x] =>
		(case getvar v
		  of SOME(INTval i) => try(RECORD[INT i, x])
		   | SOME _ => try e'
		   | NONE => giveup())
       | RECORD _ => try e'
       | STRING s => try e'
       | _ => giveup()
      end
   | HANDLE(a,b) => HANDLE(red a, red b)
   | e as INT _ => e
   | e as REAL _ => e
   | e as STRING _ => e
 and redcon (DATAcon(DATACON{rep=(VARIABLE(Access.PATH p)),const,name,typ,sign})) =
	    let fun f [v] = let val VAR w = red(VAR v) in [w] end
	          | f (i::r) = i::(f r) 
	     in DATAcon(DATACON{rep=(VARIABLE (Access.PATH(f p))),const=const,
			    name=name,typ=typ,sign=sign})
	    end
   | redcon c = c
  in red
 end


val CLOSUREval x = getvalue arg
val lam = openup x
val _ = (MCprint.printLexp lam; print "\n")
val lam' = reduce VAR lam
val _ = (MCprint.printLexp lam'; print "\n")
val lam'' = Opt.reduce(Opt.closetop lam')
val _ = (MCprint.printLexp lam''; print "\n")
 in gencode lam'' (Intmap.map free)
end


 val _ = IO.reduce_r := (print "Assigning\n"; System.cast(reopen))

end
