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

exception TagCase

fun testint i = fn INTcon j => i=j
		 | DATAcon(DATACON{rep=CONSTANT j,...}) => i=j
		 | STRINGcon j => length j = 1 andalso i = ord j
		 | DATAcon(DATACON{rep=TRANSU,...}) => true
	         | _ => false
val testboxed   = fn DATAcon(DATACON{rep=TRANSB,...}) => true
		   | DATAcon(DATACON{rep=TRANSPARENT,...}) => true
		   | DATAcon(DATACON{rep=TAGGED j,...}) => raise TagCase
		   | _ => false
fun testtag i = fn DATAcon(DATACON{rep=TRANSB,...}) => true
		 | DATAcon(DATACON{rep=TRANSPARENT,...}) => true
		 | DATAcon(DATACON{rep=TAGGED j,...}) => i=j
		 | _ => false
fun teststring s = fn STRINGcon j => s=j
		    | _ => false

fun switch(test,l,d) =
	let fun f ((c,x)::r) = if test c then x else f r
              | f nil = case d
			  of SOME z => z
			   | NONE => impossible "no default"
         in f l
        end


fun reopen (arg : 'a -> 'b) =
let val _ = print "reopening!\n"
    val free : object Intmap.intmap = Intmap.new()
    val addvalue = Intmap.add free
    fun getvar v = SOME(getvalue(Intmap.map free v))
		    handle Intmap.Intmap => NONE

fun uncompile obj =
             let val x = mkLvar()
              in addvalue(x,obj); VAR x
             end

fun openup (obj, id) =
  let val (exp,env) = reOpenLookup id
      fun findfree (v:int) =
	(case Codenvl.lookup(env,v)
         of PATH(0::l) => uncompile(followpath(obj,l))
          | _ => impossible "funny access in re-open"
	  ) handle Notfound_Codenv _ => impossible "notfound in re-open"
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
		    if pure a8
		     then red(APP(openup clo, a8))
		     else APP(f', red a8)
	          | _ => APP(f', red a8)))
   | RECORD l => RECORD(map red l)
   | SELECT(i,RECORD l) => red(nth(l,i))
   | SELECT(i, x) => 
	   (case red x
	     of RECORD l => nth(l,i)
	      | VAR w => (case getvar w
			   of SOME(RECORDval (n, obj)) => 
				uncompile(followpath(obj, [i,0]))
			    | _ => e)
	      | a7 => SELECT(i,a7))
   | RAISE x => RAISE(red x)
   | SWITCH(INT i, l, d) => red(switch(testint i, l, d))
   | SWITCH(RECORD[INT i,_], l, d) => red(switch(testtag i, l, d))
   | SWITCH(RECORD _, l, d) => red(switch(testboxed, l, d))
   | SWITCH(STRING s, l, d) => red(switch(teststring s, l, d))
   | SWITCH(VAR v, l, d) =>
	    (case getvar v
	      of SOME(RECORDval(2,obj)) =>
		       (case getvalue(followpath(obj,[1,0]))
			 of INTval i => red(switch(testtag i, l, d))
			  | _ => (red(switch(testboxed, l, d))
				  handle TagCase => e))
	       | SOME(RECORDval(n,obj)) => red(switch(testboxed, l, d))
	       | SOME(STRINGval s) => red(switch(teststring s, l, d))
	       | SOME(INTval i) => red(switch(testint i, l, d))
	       | _ => SWITCH(VAR v, map(fn(c,x)=>(c,red x)) l,
			     case d of SOME d' => SOME(red d')
				     | NONE => NONE)
	    )
   | SWITCH(e, l, d) => SWITCH(red e, map(fn(c,x)=>(c,red x)) l,
			     case d of SOME d' => SOME(red d')
				     | NONE => NONE)
   | _ => e

  in red
 end


val CLOSUREval x = getvalue arg
val lam = openup x
val _ = MCprint.printLexp lam
val lam' = reduce VAR lam
val _ = MCprint.printLexp lam'
 in gencode(Opt.closetop lam') (Intmap.map free)
end


 val _ = IO.reduce_r := (print "Assigning\n"; reopen)

end
