(* Copyright 1989 by AT&T Bell Laboratories *)
signature OPT =
sig
  val closestr : (Access.lvar -> string) * Lambda.lexp * Access.lvar list -> Lambda.lexp
  val closetop : Lambda.lexp * Access.lvar list -> Lambda.lexp
end

structure Opt : OPT =
struct

open Access Basics Lambda

fun root [v] = v | root (_::p) = root p
  | root _ = ErrorMsg.impossible "root [] in codegen/opt";

fun freevars e =
    let val t = Intset.new()
	val set = Intset.add t
	val unset = Intset.rmv t
	val done = Intset.mem t
	val free : int list ref = ref []
	val rec mak =
	 fn VAR w => if done w then () else (set w; free := w :: !free)
	  | FN (w,b) => (set w; mak b; unset w)
	  | FIX (vl,el,b) => (app set vl; app mak (b::el); app unset vl)
	  | APP (f,a) => (mak f; mak a)
	  | SWITCH(e,l,d) => 
	      (mak e;
	       app (fn (DATAcon(DATACON{rep=(VARIABLE(PATH p)),...}),e) =>
			 (mak(VAR(root p)); mak e)
		     | (c,e) => mak e)
		   l;
	       case d of NONE => () | SOME a => mak a)
	  | RECORD l => app mak l
	  | SELECT (i,e) => mak e
	  | HANDLE (a,h) => (mak a; mak h)
	  | RAISE e => mak e
	  | INT _ => ()
	  | REAL _ => ()
	  | STRING _ => ()
	  | PRIM _ => ()
    in  mak e; !free
    end

val boot_zeroSym = Symbol.symbol "boot_zero" (* receives unit *)
val boot_oneSym = Symbol.symbol "boot_one"   (* traverses free list *)
val boot_twoSym = Symbol.symbol "boot_two"   (* final bogus arg *)

fun closestr(lookup: int->string, e:lexp, extras : int list) : lexp =
    let val fv = extras @ freevars e
	val names = map lookup fv
    in  if !System.Control.debugging
	  then app (fn s => (print s; print " ")) names
	  else ();
	FN(namedLvar boot_zeroSym,
	   RECORD
	     [fold (fn (v,f) =>
		      let val w = namedLvar boot_oneSym
		       in FN(w,APP(FN(v,APP(f,SELECT(1,(VAR w)))),
				   SELECT(0,(VAR w))))
		      end)
		   fv
		   (FN(namedLvar boot_twoSym,e)),
	      fold (fn (s,f) => RECORD[STRING s, f])
		   names
		   (RECORD [])])
    end

val lookupSym = Symbol.symbol "lookup"

fun closetop(lambda: lexp, extras: int list): lexp =
  let val fv = SortedList.uniq(extras @ freevars lambda)
      val looker = namedLvar lookupSym
   in FN(looker, 
	 fold (fn (v, f) => APP(FN(v, f), APP(VAR looker, INT v))) fv lambda)
  end

end (* structure Opt *)
